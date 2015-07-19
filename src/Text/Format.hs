{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
module Text.Format (
    makeFormat
  , format
  , Format
  ) where

import Pipes
import Pipes.ByteString 
import Data.ByteString as B
import Data.ByteString.Builder
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.ByteString.Char8 (match, anyChar, takeTill, manyTill, endOfInput, parseOnly)
import Control.Applicative
import Data.Monoid

data Format = FStr !B.ByteString !Format | FPat !B.ByteString !Format | FNil
  deriving (Show)

fparser =
  let str = FStr <$> (fst <$> match (manyTill anyChar (lookAhead fm'))) <*> fm'
      pat = FPat <$> ("${" *> takeTill (== '}') <* "}") <*> fm
      eoi = endOfInput >> return FNil
      fm = eoi <|> pat <|> str
      fm' = eoi <|> pat
  in
    pat <|> str

makeFormat ∷ B.ByteString → Either String Format
makeFormat s = parseOnly fparser s
  
format ∷ Format 
       → (B.ByteString → Builder) 
       → Builder
format FNil _ = mempty
format (FStr s n) f = byteString s <> format n f
format (FPat p n) f = f p <> format n f
