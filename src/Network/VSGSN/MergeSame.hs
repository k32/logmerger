{-# LANGUAGE UnicodeSyntax #-}
module Network.VSGSN.MergeSame (
    mergeSame
  ) where

import Pipes
import Data.ByteString as B

mergeSame ∷ (Producer SGSNBasicEntry m r) → (Producer SGSNBasicEntry m r)
mergeSame p = go