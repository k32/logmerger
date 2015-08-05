{-# LANGUAGE UnicodeSyntax #-}
module Text.LogMerger.MergeSame (
    mergeSameOrigin
  ) where

import Prelude hiding (mapM_)
import Pipes
import Data.ByteString as B
import Text.LogMerger.Types
import Text.LogMerger.Logs.Types
import Data.Foldable
-- import Control.Monad.State
import qualified Data.Map as M

mergeSameOrigin ∷ (Monad m)
                ⇒ Producer SGSNBasicEntry m a
                → Producer SGSNBasicEntry m a
mergeSameOrigin p0 = do
    n ← lift $ next p0
    case n of
      Right (e0@BasicLogEntry{
                _basic_date = d0
              , _basic_origin = o0}
            , p0') →
                loop p0' d0 $ M.fromList [(o0, e0)]
      Left r → return r
  where
    mergeEntries (BasicLogEntry d1 o1 t1) (BasicLogEntry d2 o2 t2)
      | d1 == d2 && o1 == o2 = BasicLogEntry d1 o1 $ B.concat [t1, t2]

    loop p t m = do
      n ← lift $ next p
      case n of
        Right (e@BasicLogEntry{_basic_date=t', _basic_origin=o}, p')
          | t' == t → loop p' t $ M.insertWith mergeEntries o e m
          | otherwise → do
              mapM_ yield m
              loop p' t' $ M.fromList [(o, e)]
        Left r → do
          mapM_ yield m
          return r
