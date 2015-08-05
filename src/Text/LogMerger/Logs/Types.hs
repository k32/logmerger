{-# LANGUAGE UnicodeSyntax, RankNTypes #-}
module Text.LogMerger.Logs.Types (
    LogFormat(..)
  , LogDissector
  , Origin
  , SGSNOrigin
  , SGSNBasicEntry
  , module Text.Regex
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
                  ⇒ NominalDiffTime 
                  → FilePath 
                  → Producer P.ByteString m a
                  → Producer SGSNBasicEntry m (Either String ())

data LogFormat = LogFormat {
    _dissector    ∷ LogDissector
  , _nameRegex    ∷ Regex
  , _formatName   ∷ String
  , _formatDescription ∷ String
  }
