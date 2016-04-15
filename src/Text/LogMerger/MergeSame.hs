{-# LANGUAGE UnicodeSyntax #-}
module Text.LogMerger.MergeSame (
    agressiveMergeSameOrigin
  , conservativeMergeSameOrigin
  ) where

import Prelude hiding (mapM_)
import Pipes
import Data.ByteString as B
import Text.LogMerger.Types
import Text.LogMerger.Logs.Types
import Data.Foldable
import Data.List
-- import Control.Monad.State
import qualified Data.Map as M

agressiveMergeSameOrigin ∷ (Monad m)
                         ⇒ Producer SGSNBasicEntry m a
                         → Producer SGSNBasicEntry m a
agressiveMergeSameOrigin p0 = do
    n ← lift $ next p0
    case n of
      Right (e0 @ BasicLogEntry{
                _basic_date = d0
              , _basic_origin = o0}
            , p0') →
                loop p0' d0 (M.fromList [(o0, e0)]) [o0]
      Left r → return r
  where
    mergeEntries (BasicLogEntry d1 o1 t1) (BasicLogEntry d2 o2 t2)
      | d1 == d2 && o1 == o2 = BasicLogEntry d1 o1 $ B.concat [t2, t1]

    loop p t m ł = do
      n ← lift $ next p
      case n of
        Right (e@BasicLogEntry{_basic_date=t', _basic_origin=o}, p')
          | t' == t → loop p' t (M.insertWith mergeEntries o e m) (o:ł)
          | otherwise → do
              {- Ab definition nub keeps only the first occurence of each element
                 TODO: get rid of it (O(n²)), keep order in the map instead 
               -}
              mapM_ (yield . (m M.!)) $ nub ł
              loop p' t' (M.fromList [(o, e)]) [o]
        Left r → do
          mapM_ yield m
          return r

conservativeMergeSameOrigin ∷ (Monad m)
                            ⇒ Producer SGSNBasicEntry m a
                            → Producer SGSNBasicEntry m a
conservativeMergeSameOrigin p₀ = do
    n ← lift $ next p₀
    case n of
      Right (e₀, p₀') → loop p₀' e₀
      Left r → return r
  where
    loop p e₀@(BasicLogEntry d₀ o₀ t₀) = do
      n ← lift $ next p
      case n of
        Right (e₁ @ (BasicLogEntry d₁ o₁ t₁), p')
          | d₀ == d₁ && o₀ == o₁ → loop p' $ BasicLogEntry d₀ o₀ $ B.concat [t₀, t₁]
          | otherwise → do
              yield e₀
              loop p' e₁
        Left r → do
          yield e₀
          return r
