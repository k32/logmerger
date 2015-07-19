{-# LANGUAGE UnicodeSyntax, OverloadedStrings, FlexibleContexts, 
             GADTs, Rank2Types, TemplateHaskell #-}
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
import qualified Network.VSGSN.Logs.Isp as ISP
import qualified Network.VSGSN.Logs.CLI as CLI
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
  , _headerFormat ∷ B.ByteString 
  , _lineFormat   ∷ B.ByteString
  , _lmVerbosity  ∷ Int                   -- ^ Level of debug messages
  , _g_tzInfo     ∷ Maybe String          -- ^ Use this tzinfo file by default for all files
  , _g_timeZone   ∷ Maybe NominalDiffTime -- ^ Store parsed tzinfo there
  , _g_mergeSame  ∷ Bool                  -- ^ Set whether entries of the same time and origin
                                          --   Should be merged
  , _follow       ∷ Bool                  -- ^ Imitate "tail -f" behaviour
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
  , _follow = False
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
        Left (set output, "FILE")
    , CliParam "tzinfo" (Just 'i') "Path to an SGSN-MME tzinfo file" $ 
        Left (\v  → set g_tzInfo (Just v), "FILE")
    , CliParam "timezone" (Just 'z') "Timezone (UTC ± minutes)" $ 
        Left (\v  → set g_timeZone (Just . fromIntegral . (*60) $ read v), "TIME")
    , CliParam "merge-same" (Just 'm') "Merge entries of the same origin and time" $ 
        Right (set g_mergeSame)
    , CliParam "follow" (Just 'f') "Monitor updates of log files a la 'tail -f'" $ 
        Right (set follow)
    , CliParam "header" Nothing "Header format of entries" $ 
        Left (\v → set headerFormat (fromString v), "FORMAT")
    , CliParam "line" Nothing "Line format" $ 
        Left (\v → set lineFormat (fromString v), "FORMAT")
    , CliParam "verbosity" (Just 'v') "Message verbosity" $ 
        Left (\f → set lmVerbosity (read f), "NUMBER")
    ]
  , _perFileAttrs = [
      CliParam "format" (Just 'f') "Log format (used when automatic detection fails)" $ 
        Left (\f → set format (Just f), "LOG_FORMAT")
    , CliParam "offset" (Just 'o') "Specify time offset for a log" $ 
        Left (\f → set timeOffset (secondsToDiffTime $ read f), "SECONDS")
    , CliParam "merge-same" (Just 'm') "Merge entries of the same origin and time" $
        Right (set mergeSame)
    ]
  }

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

logFormat ∷ (MonadWarning [String] String m) ⇒ Maybe String → String → m (LogFormat m a)
logFormat fmt fname =
   case isOk logFormats of
    Nothing → throwW $ "Can't determine format of " ++ fname ++ ". Skipping."
    Just a  → return a
  where isOk = case fmt of
                 Nothing → find (isJust . (flip matchRegex) fname . _nameRegex)
                 Just fmt' → find ((==fmt') . _formatName)

openLog ∷ (MonadWarning [String] String m, MonadIO m, Functor m , MonadReader GlobalVars m) ⇒
          NominalDiffTime → Input → m (Producer SGSNBasicEntry m (Either [String] ()))
openLog dt (Input{_fileName = fn, _mergeSame = mgs, _format = fmt}) = do
  LogFormat {_dissector=diss} ← logFormat fmt fn
  follow ← asks $ _follow . _cfg
  let follow' = if follow
                  then Just followInterval
                  else Nothing
  p0 ← diss dt fn <$> P.fromHandleFollow follow' <$> openFile'' fn ReadMode
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

logFormats ∷ (MonadWarning [String] String m) ⇒ [LogFormat m a]
logFormats = [ISP.logFormat, CLI.logFormat, LinRB.logFormat]

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
  , map (\LogFormat{_formatName = n, _formatDescription = d} → "  " ++ n ++ " : " ++ d)
        (logFormats ∷ [LogFormat (WarningT [String] String IO) a])
  , [
      ""
    , "EXAMPLES:"
    , "  Merge ringbuffers of all boards with ISP and CLI log:"
    , ""
    , "    logmerger -z NCB/tzinfo NCB/isp.log NCB/cli_log/*/* PM/*/log.rb"
    , ""
    , "FORMATTING:"
    , "  ${YY} - year"
    , "  ${MM} - month"
    , "  ${DD} - day"
    , "  ${hh} - hour"
    , "  ${mm} - minute"
    , "  ${ss} - second"
    , "  ${origin}  - origin"
    , "  ${s}  - severity"
    ]
  ]
