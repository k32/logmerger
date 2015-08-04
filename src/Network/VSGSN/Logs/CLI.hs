{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, GADTs #-}
module Network.VSGSN.Logs.CLI
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
    _dissector = \_ fn i → let cons' = Right () <$ i
                           in cons' >-> parse' (cliDissector fn) Beginning
  , _nameRegex = mkRegex "cli_log.[0-9]+$"
  , _formatName = "sgsn-mme-cli"
  , _formatDescription = "CLI logs of an SGSN-MME node"
  }

data MyState = Beginning | Normal
             deriving (Show)
                      
cliDissector ∷ FilePath → MyState → Parser (PResult MyState SGSNBasicEntry ())
cliDissector _ Beginning = skipAnyLine >> skipAnyLine >> return (Loop Normal)
cliDissector file Normal = do
  day ← fromGregorian <$> decimal <*> ("-" *> decimal) <*> ("-" *> decimal) <* skipSpace
  (hh, mm, ss) ← (,,) <$> decimal <*> (":" *> decimal) <*> (":" *> decimal) <* skipSpace
  many "," >> skipSpace
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
