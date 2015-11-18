{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, ScopedTypeVariables,
             NoMonomorphismRestriction, LambdaCase #-}
module Text.LogMerger.Logs.LinuxRB
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
  , anyChar
  , takeTill
  )
import Data.Attoparsec.Combinator
import Data.Attoparsec.Combinator.Skip
import qualified Data.ByteString as B
import Text.LogMerger.Types
import Text.LogMerger.Logs.Types
import Text.LogMerger.Logs.Util

logFormat ∷ LogFormat
logFormat = LogFormat {
    _dissector = myDissector
  , _nameRegex = mkRegex "log\\.rb(\\.old)?$"
  , _formatName = "sgsn-mme-linux-rb"
  , _formatDescription = "Ringbuffer of SGSN-MME boards running Linux"
  }

type Tags = M.Map Word32 B.ByteString

myDissector ∷ LogDissector
myDissector = evalStateT dissect
  where dissect = parse getTags >>= \case
                    Left x → return $ Left x
                    Right tags → do
                      parse $ manyTill anyChar (lookAhead entryHead)
                      tillEnd (entry tags)

line s = string s >> endOfLine

hex' = skipSpace *> hexadecimal <* skipSpace
{-# INLINABLE hex' #-}
dec' = skipSpace *> decimal <* skipSpace
{-# INLINABLE dec' #-}

entryHead ∷ Parser (Int, Word32)
entryHead = (,) <$> ("~RB03~[" *> hex')
                <*> (hex' <* "]")

entry ∷ Tags
      → Parser SGSNBasicEntry
entry tags = do
  (d, t) ← entryHead <?> "RB_entry_head"
  txt ← matchManyTill anyChar (() <$ entryHead <|> endOfInput)
  let t'' = case M.lookup t tags of
              Nothing → "???"
              Just "undefined_tag" → "tag #" ++ show t
              Just x → fmap (toEnum . fromIntegral) $ B.unpack x
  return BasicLogEntry {
      _basic_date = posixSecondsToUTCTime (fromIntegral d)
    , _basic_origin = [OData t'']
    , _basic_text = txt
    }

getTags ∷ Parser Tags
getTags = do
  line "RB03" <?> "Ringbuffer header"
  nTags ← "Dump of" *> dec' <* "tags\n" <?> "tag count"
  replicateM_ 2 skipAnyLine <?> "tags header"
  let tag = (,) <$> (hex' <?> "tag_id")
                <*> ((takeTill (=='\n') <* endOfLine) <?> "tag_name") <?> "tag"
  M.fromList <$> replicateM nTags tag
