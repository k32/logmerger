{-|
Module      : Pipes.Dissect
Description : Split a character stream into pieces using an Attoparsec parser 
License     : Public domain
Stability   : experimental

'Pipes.Dissect' is a dumber version of pipes-parse's lens. Maybe some day it will be 
removed in favor the said library. 

Pipes.Dissect splits contents of a 'Producer' into pieces and sends them downstream.
Dissectors are useful when one needs to extact meaningful parts from a bytestream.

Dissectors are connected to Producers using 'evalStateT'. Example:

@
import Pipes
import Data.ByteString.Attoparsec
import Pipes.Dissect
import Prelude hiding (print)

data Foo = Foo | Bar 
  deriving (Show)

myParser = ("Foo" >> return Foo) 
       <|> ("Bar" >> return Bar)

main = runEffect $ print ~< parse myParser `evalStateT` yield "FooFooFooBarBarFoo"
@

``-}
{-# LANGUAGE UnicodeSyntax, RankNTypes #-}
module Pipes.Dissect (
    Dissector
  , parse
  , yieldD
  , module S
  , eof
  , tillEnd
  ) where

import Control.Monad.Trans.State.Strict as S
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import Pipes

-- | 'Dissector' is a 'Pipe'-like wrapper for an attoparsec parser
type Dissector a m r = ∀ x. StateT (Producer B.ByteString m x) (Producer a m) r

-- | 'eof' returns 'True' when the upstream producer is exhausted
eof ∷ (Monad m) 
    ⇒ Dissector a m Bool
eof = StateT $ \s → do
                 n ← lift $ next s
                 return $ case n of
                   Left _ → (True, s)
                   Right (o, s') → (False, yield o >> s')
{-# INLINABLE eof #-}

-- | Turn an attoparsec parser into 'Dissector'
parse ∷ (Monad m) 
      ⇒ A.Parser b
      → Dissector a m (Either String b)
parse p = StateT (\s → do
                    n ← lift $ next s
                    case n of
                      Left _ → return (A.parseOnly p B.empty, s)
                      Right (o, s') → go s' $ A.parse p o)
  where go s (A.Partial p') = do
            n' ← lift $ next s
            case n' of
              Left _ → return $ (A.eitherResult $ p' B.empty, s)
              Right (o, s') → go s' (p' o)
        go s f@(A.Fail l _ _) = return (A.eitherResult f, yield l >> s)
        go s f@(A.Done l _) = return (A.eitherResult f, yield l >> s)

-- | Yield a value downstream
yieldD ∷ (Monad m) 
       ⇒ a
       → Dissector a m ()
yieldD = lift . yield
{-# INLINABLE yieldD #-}

-- | Apply the same parser untill the upstream producer is exhausted
tillEnd ∷ (Monad m) 
        ⇒ A.Parser a
        → Dissector a m (Either String ())
tillEnd p = do
  n ← parse p
  case n of
    Left r → do
      e ← eof
      return $ if e
        then Right ()
        else Left r 
    Right a → yieldD a >> tillEnd p
