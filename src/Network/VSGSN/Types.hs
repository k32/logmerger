{-# LANGUAGE UnicodeSyntax, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module Network.VSGSN.Types (
   BasicLogEntry(..)
 , basic_origin
 , basic_date
 , basic_text
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
import Control.Lens

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
  } deriving (Eq, Data, Typeable)
makeLenses ''BasicLogEntry

instance (Eq o) ⇒ Ord (BasicLogEntry o) where
  compare a b = compare (_basic_date a) (_basic_date b)

deriving instance (Show o) ⇒ Show (BasicLogEntry o)

instance (Show o) ⇒ Show (Origin o) where
  show (Location {_file = f}) = f
  show (OCons {_odata = o, _onext = n}) = (show n) ++ ':':(show o)
