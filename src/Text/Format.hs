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
import Data.String

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

unescape ∷ String → B.ByteString
unescape = fromString . u 
  where u [] = []
        u ('\\':'n':t) = '\n': u t
        u (x:t) = x : u t

makeFormat ∷ String → Either String Format
makeFormat = parseOnly fparser . unescape

format ∷ Format 
       → (B.ByteString → Builder) 
       → Builder
format FNil _ = mempty
format (FStr s n) f = byteString s <> format n f
format (FPat p n) f = f p <> format n f
