{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
module Pipes.Interleave(
   interleave
 , interleaveBy
 )
 where

import Pipes
import Control.Monad
import Control.Monad.Warning
import Control.Applicative
import Data.List
import Data.Function
import Data.Either

interleaveBy ∷ (Monad m) 
             ⇒ (a → a → Ordering)
             → [Producer a m r]
             → Producer a m [r]
interleaveBy c p = do
  nn ← mapM (lift . next) p
  let vv = rights nn
      r0 = lefts nn
  go r0 $ sort' vv
  where
    sort' = sortBy (c `on` fst)
    
    go ret [] = return ret
    go ret ((v, h):t) = do
      yield v
      h' ← lift $ next h
      case h' of
       Left r → go (r:ret) t
       Right v' → go ret $ sort' $ v':t
          
interleave ∷ (Ord a, Monad m) ⇒ [Producer a m r] → Producer a m [r]
interleave = interleaveBy compare
