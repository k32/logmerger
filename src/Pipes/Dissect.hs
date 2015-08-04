{-# LANGUAGE UnicodeSyntax, RankNTypes #-}
module Pipes.Dissect (
    Dissector
  , parse
  , yieldD
  , module S
  ) where

import Control.Monad.Trans.State.Strict as S
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import Pipes

type Dissector a m r = ∀ x. StateT (Producer B.ByteString m x) (Producer a m) r

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

yieldD ∷ (Monad m) 
       ⇒ a
       → Dissector a m ()
yieldD = lift . yield
{-# INLINABLE yieldD #-}