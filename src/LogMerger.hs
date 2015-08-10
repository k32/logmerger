{-# LANGUAGE UnicodeSyntax, OverloadedStrings, FlexibleContexts, 
             GADTs, Rank2Types, TemplateHaskell, LambdaCase #-}
import Text.LogMerger
import qualified Text.LogMerger.Logs.Isp as ISP
import qualified Text.LogMerger.Logs.CLI as CLI
-- import qualified Text.LogMerger.Logs.FMAlarm as FMA
import qualified Text.LogMerger.Logs.LinuxRB as LinRB

logFormats ∷ [LogFormat]
logFormats = [ISP.logFormat, CLI.logFormat, LinRB.logFormat]

main ∷ IO ()
main = cliMergerMain cliDefaults logFormats