{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, GADTs #-}
module Text.LogMerger.Logs.CLI (
   logFormat
 ) where

import Data.Attoparsec.ByteString.Char8 (
    skipSpace
  , decimal
  , signed
  , isEndOfLine
  , endOfLine
  , char8
  , takeTill
  )
import qualified Data.ByteString.Lazy.Internal as B
import Text.LogMerger.Types
import Text.LogMerger.Logs.Types
import Text.LogMerger.Logs.Util
import Pipes
import Pipes.Dissect
import Prelude hiding (takeWhile)

logFormat ∷ LogFormat
logFormat = LogFormat {
    _dissector = cliDissector 
  , _nameRegex = mkRegex "cli_log\\.[0-9]+$"
  , _formatName = "sgsn-mme-cli"
  , _formatDescription = "CLI logs of an SGSN-MME node"
  }
                      
cliDissector ∷ LogDissector
cliDissector _ fn p0 = diss `evalStateT` p0
  where diss = tillEnd $ do  
          day ← yymmdd <* skipSpace
          time ← hhmmss <* ","
          txt ← takeTill (=='\n') <* endOfLine
          return BasicLogEntry {
              _basic_origin = Location {
                   _file = fn
                 }
            , _basic_date = UTCTime {
                  utctDay     = day
                , utctDayTime = time
                }
            , _basic_text = txt
            }
