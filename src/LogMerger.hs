{-# LANGUAGE UnicodeSyntax, TemplateHaskell #-}
import Text.LogMerger
import qualified Text.LogMerger.Logs.Isp as ISP
import qualified Text.LogMerger.Logs.CLI as CLI
-- import qualified Text.LogMerger.Logs.FMAlarm as FMA
import qualified Text.LogMerger.Logs.LinuxRB as LinRB
import Control.Lens

data VSGSNCfg i = VSGSNCfg {
    _logMerger ∷ LogMerger i
  , _tzInfo    ∷ Maybe String
  }
makeLenses ''VSGSNCfg

vsgsnDefaults = VSGSNCfg {
    _logMerger = cliDefaults
  , _tzInfo = Nothing
  }

logFormats ∷ [LogFormat]
logFormats = [ISP.logFormat, CLI.logFormat, LinRB.logFormat]

main ∷ IO ()
main = cliMergerMain logMerger vsgsnDefaults logFormats
