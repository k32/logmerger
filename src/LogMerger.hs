{-# LANGUAGE UnicodeSyntax, OverloadedStrings, FlexibleContexts, 
             GADTs, Rank2Types, TemplateHaskell, LambdaCase #-}
import Pipes
import Pipes.Interleave
import qualified Data.ByteString as B
import qualified Pipes.ByteString as P
import System.IO
import Text.CLI
import qualified Text.Format as Fmt
import Network.VSGSN.Types
import Network.VSGSN.TzInfo
import Network.VSGSN.Logs.Util
import Network.VSGSN.Logs.Types
import Network.VSGSN.PrettyPrint
import Network.VSGSN.MergeSame
-- import qualified Network.VSGSN.Logs.Isp as ISP
-- import qualified Network.VSGSN.Logs.CLI as CLI
-- import qualified Network.VSGSN.Logs.FMAlarm as FMA
import qualified Network.VSGSN.Logs.LinuxRB as LinRB
import Control.Monad
import Control.Monad.Warning
import Control.Applicative
import Control.Arrow
import Data.String
import Data.IORef
import Data.Function
import Data.Either
import Data.List
import Data.Time
import Data.Either (lefts)
import Data.Maybe
import Control.Monad.Reader
import Control.Lens
import Control.Lens.TH

data Input =
  Input {
    _fileName ∷ String
  , _format ∷ Maybe String
  , _timeOffset ∷ DiffTime
  , _mergeSame ∷ Bool
  } deriving (Show)
makeLenses ''Input

data LogMerger i = 
  LogMerger {
    _input        ∷ [i]                   -- ^ List of input files
  , _output       ∷ String                -- ^ Output stream
  , _headerFormat ∷ String 
  , _lineFormat   ∷ String
  , _lmVerbosity  ∷ Int                   -- ^ Level of debug messages
  , _g_tzInfo     ∷ Maybe String          -- ^ Use this tzinfo file by default for all files
  , _g_timeZone   ∷ Maybe NominalDiffTime -- ^ Store parsed tzinfo there
  , _g_mergeSame  ∷ Bool                  -- ^ Set whether entries of the same time and origin
                                          --   Should be merged
  , _g_follow     ∷ Bool                  -- ^ Imitate "tail -f" behaviour
  } deriving (Show)
makeLenses ''LogMerger

cliDefaults ∷ LogMerger i
cliDefaults = LogMerger {
    _input = []
  , _output = "-"
  , _headerFormat = defaultHeaderFormat
  , _lineFormat = defaultLineFormat
  , _lmVerbosity = 0
  , _g_tzInfo = Nothing
  , _g_timeZone = Nothing
  , _g_mergeSame = False
  , _g_follow = False
  }

mkInput name cfg = Input {
    _fileName = name
  , _timeOffset = 0
  , _mergeSame = _g_mergeSame cfg
  , _format = Nothing
  }

cliAttr ∷ CliDescr LogMerger Input
cliAttr = CliDescr {
    _globalAttrs = [
      CliParam "output" (Just 'o') "Output file ('-' for stdout)" $ 
        CliParameter (set output) "FILE"
    , CliParam "tzinfo" (Just 'i') "Path to an SGSN-MME tzinfo file" $ 
        CliParameter (g_tzInfo =§ Just) "FILE"
    , CliParam "timezone" (Just 'z') "Timezone (UTC ± minutes)" $ 
        CliParameter (g_timeZone =§ (Just . fromIntegral . (*60) . read)) "TIME"
    , CliParam "merge-same" (Just 'm') "Merge entries of the same origin and time" $ 
        CliFlag (set g_mergeSame)
    , CliParam "follow" (Just 'f') "Monitor updates of log files a la 'tail -f'" $ 
        CliFlag (set g_follow)
    , CliParam "header" Nothing "Header format of entries" $ 
        CliParameter (set headerFormat)"FORMAT"
    , CliParam "line" Nothing "Line format" $ 
        CliParameter (set lineFormat) "FORMAT"
    , CliParam "verbosity" (Just 'v') "Message verbosity" $ 
        CliParameter (lmVerbosity =§ read) "NUMBER"
    ]
  , _perFileAttrs = [
      CliParam "format" (Just 'f') "Log format (used when automatic detection fails)" $ 
        CliParameter (format =§ Just) "LOG_FORMAT"
    , CliParam "offset" (Just 'o') "Specify time offset for a log" $ 
        CliParameter (timeOffset =§ (secondsToDiffTime . read)) "SECONDS"
    , CliParam "merge-same" (Just 'm') "Merge entries of the same origin and time" $
        CliFlag (set mergeSame)
    ]
  }
  where
    s =§ f = \a → set s (f a)

followInterval ∷ Int
followInterval = 1000000

type Fin = IORef [IO ()]

data GlobalVars = GlobalVars {
    _cfg    ∷ LogMerger Input
  , _resMan ∷ Fin 
  }
makeLenses ''GlobalVars

openFile'' ∷ (MonadWarning [String] String m, MonadIO m, MonadReader GlobalVars m) ⇒
             FilePath → IOMode → m Handle
openFile'' fp fm = do
  ff ← asks _resMan
  h ← openFile' fp fm
  liftIO $ modifyIORef ff $ (hClose h:)
  return h

cleanup ∷ Fin → IO ()
cleanup ff = readIORef ff >>= sequence_ 

logFormat ∷ (MonadWarning [String] String m) ⇒ Maybe String → String → m LogFormat
logFormat fmt fname =
   case isOk logFormats of
    Nothing → throwW $ "Can't determine format of " ++ fname ++ ". Skipping."
    Just a  → return a
  where isOk = case fmt of
                 Nothing → find (isJust . (flip matchRegex) fname . _nameRegex)
                 Just fmt' → find ((==fmt') . _formatName)

-- TODO: Refactor me
openLog ∷ (MonadWarning [String] String m, MonadIO m, Functor m , MonadReader GlobalVars m) ⇒
          NominalDiffTime → Input → m (Producer SGSNBasicEntry m (Either [String] ()))
openLog dt (Input{_fileName = fn, _mergeSame = mgs, _format = fmt}) = do
  LogFormat {_dissector=diss} ← logFormat fmt fn
  follow' ← view (cfg . g_follow) >>= \case
              True → return $ follow followInterval
              False → return id
  let mergeSame = if mgs
                    then mergeSameOrigin
                    else id
  -- p0 ← diss dt fn <$> P.fromHandleFollow follow' <$> openFile'' fn ReadMode
  p0 ← mergeSame <$> diss dt fn <$> follow' <$> P.fromHandle <$> openFile'' fn ReadMode
  return $ if mgs
    then p0
    else p0 

printWarning, printError ∷ (MonadIO m) ⇒ String → m ()
printWarning = liftIO . hPutStrLn stderr . ("(Logmerger.hs) WARNING: " ++)
printError = liftIO . hPutStrLn stderr . ("(Logmerger.hs) ERROR: " ++)

printInfo ∷ (MonadIO m, MonadReader GlobalVars m) ⇒ Int → String → m ()
printInfo s m = do
  v ← asks $ _lmVerbosity . _cfg
  when (v >= s) $ liftIO . hPutStrLn stderr $ "(Logmerger.hs) INFO: " ++ m

runApp ∷ r → WarningT [String] String (ReaderT r IO) a → IO ()
runApp cfg a = do
  (warn, err) ← runReaderT (runWarningT a []) cfg
  mapM_ printWarning warn
  either printError (const $ return ()) err 
  
main' ∷ (MonadWarning [String] String m, MonadIO m, Functor m, MonadReader GlobalVars m) ⇒
      m ()
main' = do
  LogMerger {
      _input = inputFiles
    , _output = outputFile
    , _headerFormat = hf
    , _lineFormat = lf
    , _g_tzInfo = tzf
    , _g_timeZone = tz
    } ← asks _cfg
  pprint ← makeLogEntryPP hf lf
  localTimeOffset ← case tz of 
    Just tz' → return tz'
    Nothing  → case tzf of
      Nothing   → return 0
      Just tzf' → errorToWarning (:[]) (const $ return 0) $ readTzInfo tzf'
  printInfo 3 =<< ("Configuration: " ++) . show <$> asks _cfg
  printInfo 1 $ "POSIX time offset: " ++ (show localTimeOffset)
  sink ← case outputFile of
    "-" → return P.stdout
    _   → P.toHandle <$> openFile'' outputFile WriteMode
  logs ← errorsToWarnings (:[]) $ map (openLog localTimeOffset) inputFiles
  {- Building the pipeline -}
  let merged = interleaveBy (compare `on` _basic_date) logs
      parsingErrors r = do
        lift . printInfo 2 $ "Parsers returned: " ++ show r
        lift . warning . map show $ lefts r
  runEffect $ (parsingErrors =<< merged)
          >-> ("Broken pretty printer pipe." <! pprint)
          >-> ("Broken sink pipe." <! sink)

logFormats ∷ [LogFormat]
logFormats = [{- ISP.logFormat, CLI.logFormat, -} LinRB.logFormat]

main ∷ IO ()
main = do
  files ← newIORef []
  let stuff i = over input (i:)
  cli ← parseCli helpPreamble helpPostamble cliDefaults mkInput stuff cliAttr
  let gv = GlobalVars {
                 _cfg = cli
               , _resMan = files
               }
  runApp gv main'
  cleanup files

helpPreamble ∷ String
helpPreamble = "Merge multiple time-stamped streams into one."

helpPostamble ∷ String
helpPostamble = unlines $ concat [
    ["LOG_FORMATs:"]
  , map (\LogFormat{_formatName = n, _formatDescription = d} → "  " ++ n ++ " : " ++ d) logFormats
  , [
      ""
    , "EXAMPLES:"
    , "  Merge ringbuffers of all boards with ISP and CLI log:"
    , ""
    , "    logmerger -i NCB/tzinfo NCB/isp.log NCB/cli_log/*/* PM/*/log.rb"
    , ""
    , "FORMATTING:"
    , "  ${YY} - year"
    , "  ${MM} - month"
    , "  ${DD} - day"
    , "  ${hh} - hour"
    , "  ${mm} - minute"
    , "  ${ss} - second"
    , "  ${o}  - origin"
    , "  ${s}  - severity"
    , ""
    , "NOTES:"
    , "  Please note that --merge-same flag may cause reordering of log"
    , "  entries recorded at the same time."
    ]
  ]
