{-# LANGUAGE UnicodeSyntax #-}
module Data.Attoparsec.Combinator.Skip (
    skipManyTill
  , matchManyTill
  , Skippable(..)
  ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import Data.Attoparsec.Combinator

skipManyTill ∷ Alternative f
             ⇒ f a
             → f b
             → f b
skipManyTill a end = go
  where go = end <|> (a *> go)

matchManyTill ∷ Parser a
              → Parser b
              → Parser B.ByteString
matchManyTill a end = fst <$> match (skipManyTill a (lookAhead end))

class Skippable a where
--  TODO: Add me
