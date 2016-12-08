{-# LANGUAGE UnicodeSyntax, TemplateHaskell, LambdaCase, FlexibleContexts,
             RankNTypes, OverloadedStrings #-}
module Text.LogMerger (
    module Pipes
  , module Text.LogMerger.Types
  , module Text.LogMerger.Logs.Types
  , Input(..)
  , LogMerger(..)
  , LogMergerCfg
  , cliDefaults
  , cliMergerMain
  ) where

import Pipes
import qualified Pipes.Prelude as PPr
import Pipes.Interleave
import qualified Pipes.ByteString as P
import Text.CLI
import qualified Text.Format as Fmt
import Text.LogMerger.Types
import Text.LogMerger.PrettyPrint
import Text.LogMerger.MergeSame
import Text.LogMerger.TzInfo
import Text.LogMerger.Logs.Util
import Text.LogMerger.Logs.Types
import qualified Data.ByteString as B
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.Warning
import Control.Monad.Reader
import Data.IORef
import Data.List (find, isPrefixOf)
import Data.Maybe (isJust)
import Data.Function (on)
import Data.Either (lefts, rights)
import qualified Data.ByteString as B
import Text.Read (readMaybe)
import System.IO
import Debug.Trace

data MergeSame = NoMergeSame | ConservativeMerge | AgressiveMerge
               deriving (Show)

data Input =
  Input {
    _fileName ∷ String
  , _format ∷ Maybe String
  , _timeOffset ∷ NominalDiffTime
  , _mergeSame ∷ MergeSame
  } deriving (Show)
makeLenses ''Input

readMergeSame :: String -> MergeSame
readMergeSame "" = NoMergeSame
readMergeSame a | isPrefixOf a "conservative" = ConservativeMerge
                | isPrefixOf a "agressive"    = AgressiveMerge
                | otherwise                   = error "LogMerger: --merge-same parameter should be either \"conservative\" or \"agressive\""

data LogMerger i = 
  LogMerger {
    _input            ∷ [i]                   -- ^ List of input files
  , _output           ∷ String                -- ^ Output stream
  , _headerFormat     ∷ String 
  , _lineFormat       ∷ String
  , _lmVerbosity      ∷ Int                   -- ^ Level of debug messages
  , _g_tzInfo         ∷ Maybe String          -- ^ Use this tzinfo file by default for all files
  , _g_timeZone       ∷ Maybe String          -- ^ Timezone (difference between UTC and localtime in [+-]hhmm format)
  , _g_mergeSame      ∷ MergeSame             -- ^ Set whether entries of the same time and origin
                                               --   Should be merged
  , _g_follow         ∷ Bool                  -- ^ Imitate "tail -f" behaviour
  , _g_followInterval ∷ Int                   -- ^ How often input files should be refreshed
  } deriving (Show)
makeLenses ''LogMerger

type LogMergerCfg = LogMerger Input

cliAttr ∷ ASetter' (s0 Input) (LogMerger Input)
        → CliDescr s0 Input
cliAttr s0 = CliDescr {
    _globalAttrs = process [
      CliParam "output" (Just 'o') "Output file ('-' for stdout)" $ 
        CliParameter (output :$ id) "FILE"
    , CliParam "tzinfo" (Just 'i') "Path to an SGSN-MME tzinfo file" $ 
        CliParameter (g_tzInfo :$ Just) "FILE"
    , CliParam "timezone" (Just 'z') "Timezone (UTC ± minutes)" $ 
        CliParameter (g_timeZone :$ Just) "TIME"
    , CliParam "merge-same" (Just 'm') "Merge entries of the same origin and time" $ 
        CliParameter (g_mergeSame :$ readMergeSame) "c[onservative] or a[gressive]"
    , CliParam "follow" (Just 'f') "Monitor updates of log files a la 'tail -f'" $ 
        CliFlag g_follow
    , CliParam "header" Nothing "Header format of entries" $ 
        CliParameter (headerFormat :$ id)"FORMAT"
    , CliParam "line" Nothing "Line format" $ 
        CliParameter (lineFormat :$ id) "FORMAT"
    , CliParam "verbosity" (Just 'v') "Message verbosity" $ 
        CliParameter (lmVerbosity :$ read) "NUMBER"
    ]
  , _perFileAttrs = [
      CliParam "format" (Just 'f') "Log format (used when automatic detection fails)" $ 
        CliParameter (format :$ Just) "LOG_FORMAT"
    , CliParam "offset" (Just 'o') "Specify time offset for a log" $ 
        CliParameter (timeOffset :$ (fromInteger . read)) "SECONDS"
    , CliParam "merge-same" (Just 'm') "Merge entries of the same origin and time" $
        CliParameter (mergeSame :$ readMergeSame) "c[onservative] or a[gressive]"
    ]
  }
  where
    process = (mapped . setter)  %~ (.→ s0)

mkInput l0 name cfg = Input {
    _fileName = name
  , _timeOffset = 0
  , _mergeSame = cfg ^. l0 . g_mergeSame
  , _format = Nothing
  }

cliDefaults ∷ LogMerger i
cliDefaults = LogMerger {
    _input = []
  , _output = "-"
  , _headerFormat = defaultHeaderFormat
  , _lineFormat = defaultLineFormat
  , _lmVerbosity = 0
  , _g_tzInfo = Nothing
  , _g_timeZone = Nothing
  , _g_mergeSame = NoMergeSame
  , _g_follow = False
  , _g_followInterval = 1000000
  }

type Fin = IORef [IO ()]

data GlobalVars = GlobalVars {
    _myConfig    ∷ LogMerger Input
  , _resMan      ∷ Fin 
  }
makeLenses ''GlobalVars

openFile'' ∷ (MonadWarning [String] String m, MonadIO m, MonadReader GlobalVars m) 
           ⇒ FilePath 
           → IOMode 
           → m Handle
openFile'' fp fm = do
  ff ← asks _resMan
  h ← openFile' fp fm
  liftIO $ modifyIORef ff $ (hClose h:)
  return h

cleanup ∷ Fin 
        → IO ()
cleanup ff = readIORef ff >>= sequence_ 

logFormat ∷ (MonadWarning [String] String m) 
          ⇒ [LogFormat]
          → Maybe String
          → String 
          → m LogFormat
logFormat logFormats fmt fname =
   case isOk logFormats of
    Nothing → throwW $ "Can't determine format of " ++ fname ++ ". Skipping."
    Just a  → return a
  where isOk = case fmt of
                 Nothing → find (isJust . (flip matchRegex) fname . _nameRegex)
                 Just fmt' → find ((==fmt') . _formatName)

refactorMe ∷ (Monad m) ⇒ (a → b) → Producer a m r → Producer b m r
refactorMe f p = do
  n ← lift $ next p
  case n of
    Left r → return r
    Right (x, p') → do
      yield $ f x
      refactorMe f p'

parseTimeZone ∷ String → Maybe NominalDiffTime
parseTimeZone tz@ ~(sign:h1:h2:mm) = do
  guard $ 5 == length tz
  sign' ← case sign of
            '-' → return '-'
            '+' → return ' ' -- TODO: Hack hack hack
            _   → Nothing
  hh ← (readMaybe [sign',h1,h2]) :: Maybe Integer
  mm ← (readMaybe mm) :: Maybe Integer
  guard $ mm <= 60 && abs hh <= 23
  return $ realToFrac $ secondsToDiffTime $ (hh*60 + mm) * 60

-- TODO: Refactor me
openLog ∷ (MonadWarning [String] String m, MonadIO m, Functor m, MonadReader GlobalVars m)
        ⇒ [LogFormat]
        → NominalDiffTime
        → NominalDiffTime
        → Input 
        → m (Producer SGSNBasicEntry m (Either String ()))
openLog logFormats unixTimeOffset timezone inputSpec = do
  let Input{ _fileName = fn
           , _mergeSame = mgs
           , _format = fmt
           , _timeOffset = dt
           } = inputSpec
  LogFormat { _dissector = diss
            , _timeAs = timeAs
            } ← logFormat logFormats fmt fn
  -- Follow pipe determines whether file should be read periodically or just once
  follow' ← view (myConfig . g_follow) >>= \case
              True → follow <$> view (myConfig . g_followInterval)
              False → return id
  let mergeSamePipe = case mgs of
                        AgressiveMerge -> agressiveMergeSameOrigin
                        ConservativeMerge -> conservativeMergeSameOrigin
                        NoMergeSame -> id
      -- Post process pipe does time conversion and adds filename to the origin
      postProcessPipe = refactorMe $ case timeAs of
                                       AsLocalTime → asLocalTime dt fn
                                       AsUnixTime → asUnixTime unixTimeOffset dt fn
                                       AsUTC → asUTCTime timezone dt fn
  -- p0 ← diss dt fn <$> P.fromHandleFollow follow' <$> openFile'' fn ReadMode
  p0 ← mergeSamePipe <$> postProcessPipe <$> diss <$> follow' <$>
         P.fromHandle <$> openFile'' fn ReadMode
  return p0

asLocalTime ∷ NominalDiffTime
            → String
            → SGSNBasicEntry
            → SGSNBasicEntry
asLocalTime dt fn e@BasicLogEntry{_basic_origin = o, _basic_date = d} =
  e{_basic_origin = Location fn : o, _basic_date = dt `addUTCTime` d}

asUnixTime ∷ NominalDiffTime
           → NominalDiffTime
           → String
           → SGSNBasicEntry
           → SGSNBasicEntry
asUnixTime udt dt fn e@BasicLogEntry{_basic_origin = o, _basic_date = d} =
  e{_basic_origin = Location fn : o, _basic_date = (dt + udt) `addUTCTime` d}

asUTCTime ∷ NominalDiffTime
          → NominalDiffTime
          → String
          → SGSNBasicEntry
          → SGSNBasicEntry
asUTCTime tz dt fn e@BasicLogEntry{_basic_origin = o, _basic_date = d} =
  e{_basic_origin = Location fn : o, _basic_date = (dt + tz) `addUTCTime` d}

printWarning, printError ∷ (MonadIO m) ⇒ String → m ()
printWarning = liftIO . hPutStrLn stderr . ("(Logmerger.hs) WARNING: " ++)
printError = liftIO . hPutStrLn stderr . ("(Logmerger.hs) ERROR: " ++)

printInfo ∷ (MonadIO m, MonadReader GlobalVars m) ⇒ Int → String → m ()
printInfo s m = do
  v ← asks $ _lmVerbosity . _myConfig
  when (v >= s) $ liftIO . hPutStrLn stderr $ "(Logmerger.hs) INFO: " ++ m

runApp ∷ r → WarningT [String] String (ReaderT r IO) a → IO ()
runApp cfg a = do
  (warn, err) ← runReaderT (runWarningT a []) cfg
  mapM_ printWarning warn
  either printError (const $ return ()) err 
  
cliMergerMain' ∷ (MonadWarning [String] String m, MonadIO m, Functor m, MonadReader GlobalVars m) 
               ⇒ [LogFormat] 
               → m ()
cliMergerMain' logFormats = do
  LogMerger {
      _input = inputFiles
    , _output = outputFile
    , _headerFormat = hf
    , _lineFormat = lf
    , _g_tzInfo = tzf
    , _g_timeZone = tz
    } ← asks _myConfig
  pprint ← makeLogEntryPP hf lf
  localTimeOffset ← case tzf of
                      Nothing   → return 0
                      Just tzf' → errorToWarning (:[]) (const $ return 0) $ readTzInfo tzf'
  printInfo 3 =<< ("Configuration: " ++) . show <$> asks _myConfig
  printInfo 1 $ "POSIX time offset: " ++ (show localTimeOffset)
  sink ← case outputFile of
    "-" → return P.stdout
    _   → P.toHandle <$> openFile'' outputFile WriteMode
  timezone ← case (tz, tz >>= parseTimeZone) of
               (Nothing, _) → return 0
               (Just tz', Nothing) → do
                 printWarning $ "Could not parse time zone: " ++ tz'
                 return 0
               (_, Just tz') → return tz'
  printInfo 1 $ "Timezone offset: " ++ (show timezone)
  logs ← errorsToWarnings (:[]) $ map (openLog logFormats localTimeOffset timezone) inputFiles
  {- Building the pipeline -}
  let merged = interleaveBy (compare `on` _basic_date) logs
      parsingErrors r = do
        lift . printInfo 2 $ "Parsers returned: " ++ show r
        lift . warning . map show $ lefts r
  {- Emacs-specific hack, to be removed when proper solution is implemented -}
  runEffect $ yield "-*- mode: logmerger-*-" >-> sink 
  runEffect $ (parsingErrors =<< merged)
          >-> ("Broken pretty printer pipe." <! pprint)
          >-> ("Broken sink pipe." <! sink)

cliMergerMain ∷ Lens' (s0 Input) (LogMerger Input)
              → s0 Input                               -- ^ Default options
              → [LogFormat]                            -- ^ List of supported log formats
              → IO ()
cliMergerMain s0 cfg0 logFormats = do
  files ← newIORef []
  let stuff i = s0 . input %~ (i:)
  cli ← parseCli helpPreamble
                 (helpPostamble logFormats)
                 cfg0
                 (mkInput s0)
                 stuff
                 (cliAttr s0)
  let gv = GlobalVars {
                 _myConfig = cli ^. s0
               , _resMan = files
               }
  runApp gv $ cliMergerMain' logFormats
  cleanup files

helpPreamble ∷ String
helpPreamble = "Merge multiple time-stamped streams into one."

helpPostamble ∷ [LogFormat] 
              → String
helpPostamble logFormats = unlines $ concat [
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
    , "  Please note that '--merge-same agressive' may cause reordering of log"
    , "  entries recorded at the same time."
    ]
  ]

