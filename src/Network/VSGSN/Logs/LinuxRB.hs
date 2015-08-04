{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, ScopedTypeVariables,
             NoMonomorphismRestriction, LambdaCase #-}
module Network.VSGSN.Logs.LinuxRB
       (
         logFormat
       ) where

import Prelude hiding (mapM_, takeWhile)
import Pipes.Dissect
import Data.Traversable
import qualified Data.Map as M
import Data.Word (Word64, Word32)
import Data.Time.Clock.POSIX
import Control.Applicative
import Control.Monad
import Control.Monad.Warning
import Data.Attoparsec.ByteString.Char8 (
    hexadecimal
  , decimal
  , endOfLine
  , skipSpace
  , string
  , Parser
  , match
  , anyChar
  , takeTill
  )
import Data.Attoparsec.Combinator
import qualified Data.ByteString as B
import Network.VSGSN.Types
import Network.VSGSN.Logs.Types
import Network.VSGSN.Logs.Util

logFormat ∷ LogFormat
logFormat = LogFormat {
    _dissector = myDissector
  , _nameRegex = mkRegex "log\\.rb(\\.old)?$"
  , _formatName = "sgsn-mme-linux-rb"
  , _formatDescription = "Ringbuffer of SGSN-MME boards running Linux"
  }

type Tags = M.Map Word32 B.ByteString

myDissector ∷ LogDissector
myDissector dt fn p0 = dissect `evalStateT` p0
  where dissect = parse getTags >>= \case
                    Left x → return $ Left [x]
                    Right tags → do
                      parse $ manyTill anyChar (lookAhead entryHead)
                      loop tags
        loop tags = parse (entry dt fn tags) >>= \case
                      Left a → return $ Left [a]
                      Right e → do
                        yieldD e
                        loop tags

line s = string s >> endOfLine

hex' = skipSpace *> hexadecimal <* skipSpace
{-# INLINABLE hex' #-}
dec' = skipSpace *> decimal <* skipSpace
{-# INLINABLE dec' #-}

entryHead ∷ Parser (Int, Word32)
entryHead = (,) <$> ("~RB03~[" *> hex')
                <*> (hex' <* "]")

entry ∷ NominalDiffTime 
      → String 
      → Tags 
      → Parser SGSNBasicEntry
entry dt fn tags = do
  (d, t) ← entryHead <?> "RB_entry_head"
  (txt, _) ← match $ manyTill anyChar $ lookAhead (() <$ entryHead <|> endOfInput)
  let t'' = case M.lookup t tags of
              Nothing → "???"
              Just "undefined_tag" → "tag #" ++ show t
              Just x  → fmap (toEnum . fromIntegral) $ B.unpack x
  return BasicLogEntry {
      _basic_date = dt `addUTCTime` posixSecondsToUTCTime (fromIntegral d)
    , _basic_origin = t'' `OCons` Location fn
    , _basic_text = txt
    }

getTags ∷ Parser Tags
getTags = do
  line "RB03" <?> "Ringbuffer header"
  nTags ← "Dump of" *> dec' <* "tags\n" <?> "tag count"
  replicateM_ 2 skipAnyLine <?> "tags header"
  let tag = (,) <$> (hex' <?> "tag_id") <*> ((takeTill (=='\n') <* endOfLine) <?> "tag_name") <?> "tag"
  M.fromList <$> replicateM nTags tag