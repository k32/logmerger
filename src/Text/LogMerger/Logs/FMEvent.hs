{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections #-}
module Text.LogMerger.Logs.FMEvent
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
import Text.LogMerger.Logs.FMAlarm as FMA
import Prelude hiding (takeWhile)

logFormat = LogFormat {
    _dissector = _dissector FMA.logFormat
  , _nameRegex = mkRegex "fm_event.[0-9]+$"
  , _formatName = "fm_event"
  , _formatDescription = "Log of Fault Management event of an SGSN-MME node"
  }
