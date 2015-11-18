{-# LANGUAGE UnicodeSyntax, RankNTypes #-}
module Text.LogMerger.Logs.Types (
    LogFormat(..)
  , LogDissector
  , Origin
  , SGSNOrigin
  , SGSNBasicEntry
  , module Text.Regex
  , module Text.LogMerger.Types
  ) where

import Pipes
import Data.Time (NominalDiffTime)
import qualified Pipes.ByteString as P
import Text.Regex
import Text.LogMerger.Types
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Lazy.Internal as B

type SGSNOrigin = String

type SGSNBasicEntry = BasicLogEntry SGSNOrigin

type LogDissector = ∀ m a . (Monad m) 
                  ⇒ Producer P.ByteString m a
                  → Producer SGSNBasicEntry m (Either String ())

data LogFormat = LogFormat {
    _dissector    ∷ LogDissector  -- ^ Parser splitting bytestream to entries
  , _nameRegex    ∷ Regex         -- ^ Regex used to pick dissector for a file
  , _formatName   ∷ String        -- ^ Name of log format, used in manual parser picking
  , _formatDescription ∷ String   -- ^ Description of the log format
  , _timeAs       ∷ TimeAs        -- ^ How to interpret timestamps
  }
