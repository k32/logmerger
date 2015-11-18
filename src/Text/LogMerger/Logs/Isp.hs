{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, GADTs #-}
module Text.LogMerger.Logs.Isp
       (
         logFormat
       ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 (
    skipSpace
  , decimal
  , signed
  , takeTill
  , isEndOfLine
  , endOfLine
  , endOfInput
  , char8
  , (<?>)
  , Parser
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
    _dissector = ispDissector
  , _nameRegex = mkRegex "isp\\.log$"
  , _formatName = "sgsn-mme-isp"
  , _formatDescription = "Isp log of SGSN-MME nodes"
  , _timeAs = AsLocalTime
  }

logEntry ∷ Parser SGSNBasicEntry
logEntry = do
  day ← yymmdd <* skipSpace
  time ← hhmmss <* skipSpace
  off ← "UTC" *> signed decimal <* (skipSpace >> ";") -- TODO: do something with off
  txt ← takeTill (=='\n') <* (endOfLine <|> endOfInput)
  return BasicLogEntry {
        _basic_origin = []
      , _basic_date = UTCTime {
            utctDay     = day
          , utctDayTime = time
          }
      , _basic_text = txt
      }

ispDissector ∷ LogDissector
ispDissector = evalStateT $ do
  parse ("Content of isp.log\n==================\n" <?> "isp.log header")
  tillEnd logEntry
