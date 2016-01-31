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
  , endOfInput
  , char8
  , skipWhile
  , match
  )
import qualified Data.ByteString.Lazy.Internal as B
import Text.LogMerger.Types
import Text.LogMerger.Logs.Types
import Text.LogMerger.Logs.Util
import Pipes
import Pipes.Dissect
import Prelude hiding (takeWhile)
import Control.Applicative
    
logFormat ∷ LogFormat
logFormat = LogFormat {
    _dissector = cliDissector 
  , _nameRegex = mkRegex "cli_log\\.[0-9]+$"
  , _formatName = "sgsn-mme-cli"
  , _formatDescription = "CLI logs of an SGSN-MME node"
  , _timeAs = AsLocalTime
  }
                      
cliDissector ∷ LogDissector
cliDissector = evalStateT diss
  where diss = tillEnd $ do  
          day ← yymmdd <* skipSpace
          time ← hhmmss <* ","
          (txt, _) ← match $ skipWhile (/='\n') <* (endOfLine <|> endOfInput)
          return BasicLogEntry {
              _basic_origin = []
            , _basic_date = UTCTime {
                  utctDay     = day
                , utctDayTime = time
                }
            , _basic_text = txt
            }
