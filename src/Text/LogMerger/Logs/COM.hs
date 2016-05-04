{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, RankNTypes #-}
module Text.LogMerger.Logs.COM
       (
         comLogFormat
       , comStartLogFormat
       ) where

import Pipes.Dissect as Diss
import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Internal as B
import Text.LogMerger.Types
import Text.LogMerger.Logs.Types
import Text.LogMerger.Logs.Util
import Prelude hiding (takeWhile)
import Data.Time.Clock
import Data.Attoparsec.Combinator.Skip

comLogFormat = LogFormat {
    _dissector = evalStateT comLogDissector
  , _nameRegex = mkRegex "com\\.log\\.[0-9]+$"
  , _formatName = "sgsn-mme-com-log"
  , _formatDescription = "Log of COM application at SGSN-MME node"
  , _timeAs = AsLocalTime
  }

comStartLogFormat = LogFormat {
    _dissector = evalStateT comStartLogDissector
  , _nameRegex = mkRegex "com_start\\.log\\.[0-9]+$"
  , _formatName = "sgsn-mme-com-start-log"
  , _formatDescription = "Log of COM application at SGSN-MME node"
  , _timeAs = AsLocalTime
  }

comLogDissector ∷ (Monad m)
                ⇒ Dissector SGSNBasicEntry m (Either String ())
comLogDissector = tillEnd $ do
  prefix ← takeTill (==':') <* ":" <?> "COM log prefix"
  skipWhile isSpace
  day ← (yymmdd <* skipWhile isSpace) <?> "COM Day"
  time ← hhmmss <?> "Time"
  nanoseconds ← "." *> decimal
  (rest, _) ← (match $ (takeTill (=='\n') <* "\n")) <?> "rest"
  return BasicLogEntry {
      _basic_origin = []
    , _basic_date = UTCTime {
          utctDay     = day
        , utctDayTime = time + (picosecondsToDiffTime $ 1000000*nanoseconds)
        }
    , _basic_text = B.concat [prefix, ":", rest]
    }

comStartLogDissector ∷ (Monad m)
                     ⇒ Dissector SGSNBasicEntry m (Either String ())
comStartLogDissector = dissect
  where
    entryHead = do
          day ← (yymmdd <* skipWhile isSpace) <?> "COM-start Day"
          time ← hhmmss <?> "Time"
          microseconds ← "." *> decimal
          return UTCTime {
            utctDay     = day
          , utctDayTime = time + (picosecondsToDiffTime $ 1000000000*microseconds)
          }

    entryHead' = endOfLine *> entryHead

    dissect = do
      e0 ← Diss.parse $ do
        time' ← entryHead
        txt' ← matchManyTill anyChar (() <$ entryHead' <|> endOfInput)
        return BasicLogEntry {
          _basic_origin = []
        , _basic_date = time'
        , _basic_text = txt'
        }
      case e0 of
        (Right e0') -> do
          yieldD e0'
          tillEnd $ do
            time ← entryHead'
            txt ← matchManyTill anyChar (() <$ entryHead' <|> endOfInput)
            return BasicLogEntry {
                _basic_origin = []
              , _basic_date = time
              , _basic_text = txt
            }
        (Left r) -> do
          e ← eof
          return $ if e
            then Right ()
            else Left r
