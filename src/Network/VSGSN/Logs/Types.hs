{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, KindSignatures, GADTs #-}
module Network.VSGSN.Logs.Types
       (
         LogFormat(..)
       , Dissector
       , Origin
       , SGSNOrigin
       , SGSNBasicEntry
       , module Text.Regex
       ) where

import Pipes
import Data.Time (NominalDiffTime)
import qualified Pipes.ByteString as P
import Text.Regex
import Network.VSGSN.Types
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Lazy.Internal as B

type SGSNOrigin = String

type SGSNBasicEntry = BasicLogEntry SGSNOrigin

type Dissector m a = Producer P.ByteString m a →
                     Producer SGSNBasicEntry m (Either [String] ())

data LogFormat m a
  where LogFormat ∷ {
    _dissector    ∷ NominalDiffTime → FilePath → Dissector m a
  , _nameRegex    ∷ Regex
  , _formatName   ∷ String
  , _formatDescription ∷ String
  } → LogFormat m a
