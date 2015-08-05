{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections #-}
module Text.LogMerger.Logs.FMAlarm
       (
         logFormat
       ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Internal as B
import Text.LogMerger.Types
import Text.LogMerger.Logs.Types
import Text.LogMerger.Logs.Util
import Prelude hiding (takeWhile)

logFormat ∷ (Monad m) ⇒ LogFormat m a
logFormat = LogFormat {
    _dissector = attoParsecDissector fmaDissector
  , _nameRegex = mkRegex "fm_(alarm|event).[0-9]+$"
  }

fmaDissector ∷ FilePath → Parser SGSNBasicEntry
fmaDissector file = do
  b ← count 4 $ takeTill (==';') <* ";"
  skipWhile isSpace
  day ← fromGregorian <$> decimal <*> ("-" *> decimal) <*> ("-" *> decimal) <* space
  (hh, mm, ss) ← (,,) <$> decimal <*> (":" *> decimal) <*> (":" *> decimal) <* ";"
  rest ← takeTill (=='\n') <* "\n"
  return BasicLogEntry {
      _basic_origin   = Location {
           _file = file
         }
    , _basic_date = UTCTime {
          utctDay     = day
        , utctDayTime = secondsToDiffTime $ (hh * 3600) + (mm * 60) + ss
        }
    , _basic_text = B.intercalate "; " $ b ++ [rest]
    }
