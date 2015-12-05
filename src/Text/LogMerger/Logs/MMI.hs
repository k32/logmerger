{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, ScopedTypeVariables,
             NoMonomorphismRestriction, LambdaCase #-}
module Text.LogMerger.Logs.MMI
       (
         logFormat
       ) where

import Prelude hiding (mapM_, takeWhile)
import Pipes.Dissect
import Data.Traversable
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.Warning
import Data.Attoparsec.ByteString.Char8 (
    decimal
  , endOfLine
  , skipSpace
  , string
  , Parser
  , anyChar
  , takeTill
  )
import Data.Attoparsec.Combinator
import Data.Attoparsec.Combinator.Skip
import qualified Data.ByteString as B
import Text.LogMerger.Types
import Text.LogMerger.Logs.Types
import Text.LogMerger.Logs.Util

logFormat ∷ LogFormat
logFormat = LogFormat {
    _dissector = myDissector
  , _nameRegex = mkRegex "mmi_log\\.[0-9]+$"
  , _formatName = "sgsn-mme-mmi"
  , _formatDescription = "MMI log of SGSN-MME"
  , _timeAs = AsLocalTime
  }

myDissector ∷ LogDissector
myDissector = evalStateT $ tillEnd entry

entry ∷ Parser SGSNBasicEntry
entry = do
  let header = do
        reqrepl ← ("REQUEST" <|> "REPLY") <?> "header/reqrepl"
        skipSpace
        (d, t) ← header_old <|> header_new
        return (reqrepl, d, t)
      header_old = do
        d ← ("Date:" *> yymmdd' "/" <* ",") <?> "header_old/date"
        t ← (skipSpace >> "Time:" *> hhmmss) <?> "header_old/time"
        return (d, t)
      header_new = do
        d ← yymmdd <* "T" <?> "header_new/date"
        t ← hhmmss <?> "header_new/time"
        return (d, t)
  (reqrepl, d, t) ← header
  txt ← matchManyTill anyChar (() <$ header <|> endOfInput)
  return BasicLogEntry {
      _basic_date = UTCTime {
          utctDay = d
        , utctDayTime = t
        }
    , _basic_origin = []
    , _basic_text = B.concat [reqrepl, txt]
    }
