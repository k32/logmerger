{-# LANGUAGE UnicodeSyntax, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module Network.VSGSN.Types (
   BasicLogEntry(..)
 -- , Event(..)
 , Origin(..)
 , DateTime
 , module Data.Time
 ) where

import Data.Time
import Data.Word
-- import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.ByteString as B
import Data.ByteString
import Data.Data
import Data.Typeable
import Data.ByteString.Internal (c2w, w2c)

type DateTime = UTCTime

data Origin o =
  OCons {
    _odata ∷ o
  , _onext ∷ Origin o
  }
  | Location {
    _file   ∷ FilePath
  -- TODO: We've had a problem. Looks like attoparsec doesn't keep offset
  -- , _offset ∷ Word64 
  -- , _size   ∷ Word32 
  } deriving (Data, Eq, Ord, Typeable)

data BasicLogEntry o =
  BasicLogEntry {
    _basic_date   ∷ DateTime
  , _basic_origin ∷ Origin o
  , _basic_text   ∷ B.ByteString
  } deriving (Data, Typeable)

deriving instance (Show o) ⇒ Show (BasicLogEntry o)

instance (Show o) ⇒ Show (Origin o) where
  show (Location {_file = f}) = f
  show (OCons {_odata = o, _onext = n}) = (show n) ++ ':':(show o)
