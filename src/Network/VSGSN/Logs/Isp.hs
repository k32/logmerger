{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, GADTs #-}
module Network.VSGSN.Logs.Isp
       (
         logFormat
       ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (
    skipSpace
  , decimal
  , signed
  , isEndOfLine
  , endOfLine
  , char8)
import qualified Data.ByteString.Lazy.Internal as B
import Network.VSGSN.Types
import Network.VSGSN.Logs.Types
import Network.VSGSN.Logs.Util
import Pipes
import Prelude hiding (takeWhile)

logFormat ∷ (Monad m) ⇒ LogFormat m a
logFormat = LogFormat {
    _dissector = ispDissector
  , _nameRegex = mkRegex "isp.log$"
  , _formatName = "sgsn-mme-isp"
  , _formatDescription = "Isp log of SGSN-MME nodes"
  }

ispDissector ∷ NominalDiffTime → FilePath → MyState → Parser (PResult MyState SGSNBasicEntry ())
ispDissector _  = skipAnyLine >> skipAnyLine >> return (Loop Normal)
ispDissector file Normal = do
  day ← fromGregorian <$> decimal <*> ("-" *> decimal) <*> ("-" *> decimal) <* skipSpace
  (hh, mm, ss) ← (,,) <$> decimal <*> (":" *> decimal) <*> (":" *> decimal) <* skipSpace
  off ← "UTC" *> signed decimal <* (skipSpace >> ";") -- TODO: do something with off
  txt ← takeTill isEndOfLine <* endOfLine
  return $ Yield Normal $ BasicLogEntry {
      _basic_origin   = Location {
           _file = file
         }
    , _basic_date = UTCTime {
          utctDay     = day
        , utctDayTime = secondsToDiffTime $ (hh * 3600) + (mm * 60) + ss
        }
    , _basic_text = txt
    }
