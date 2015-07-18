{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, ScopedTypeVariables,
             NoMonomorphismRestriction #-}
module Network.VSGSN.Logs.LinuxRB
       (
         logFormat
       ) where

import Prelude hiding (mapM_, takeWhile)
import Pipes
import Data.Traversable
import qualified Data.Map as M
import Data.Either (either)
import Data.Word (Word64, Word32)
import Data.Time.Clock.POSIX
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
  (
    skipSpace
  , hexadecimal
  , decimal
  , isEndOfLine
  , endOfLine
  , char8
  )
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.VSGSN.Types
import Network.VSGSN.Logs.Types
import Network.VSGSN.Logs.Util
import Debug.Trace
import Prelude hiding (takeWhile)

logFormat ∷ (Monad m) ⇒ LogFormat m a
logFormat = LogFormat {
    _dissector = \dt fn i → let cons' = Right () <$ i
                                parseEntries = parse' (rbDissector dt fn) TabulaRasa
                            in mergeSameOrigin $ cons' >-> parseEntries
  , _nameRegex = mkRegex "log\\.rb(\\.old)?$"
  , _formatName = "sgsn-mme-linux-rb"
  , _formatDescription = "Ringbuffer of SGSN-MME boards running Linux"
  }

toNextEntry ∷ Parser (B.ByteString, (Word64, Word32))
toNextEntry = loop ""
  where rbHead = (,) <$> ("~RB03~[" *> hex')
                     <*> (hex' <* "]")
        further t0 t1 = char8 '~' >> (loop $ B.concat [t0, t1, "~"]) -- TODO: Possible space leak!
        enough t0 t1 = (B.concat [t0, t1],) <$> rbHead
        loop t0 = do
          t1 ← takeTill (==c2w '~')
          enough t0 t1 <|> further t0 t1

type Tags = M.Map Word32 B.ByteString

line s = string s >> endOfLine

hex' = skipSpace *> hexadecimal <* skipSpace
{-# INLINABLE hex' #-}
dec' = skipSpace *> decimal <* skipSpace
{-# INLINABLE dec' #-}

getTags ∷ Parser Tags
getTags = do
  line "RB03" <?> "tag header"
  nTags ← "Dump of" *> dec' <* "tags\n" <?> "tag count"
  replicateM_ 2 skipAnyLine <?> "tag footer"
  let tag = (,) <$> hex' <*> (takeTill isEndOfLine <* endOfLine) <?> "tag"
  M.fromList <$> replicateM nTags tag 

data MyState = TabulaRasa
             | GotTags Tags (Word64, Word32)
             | Finish

instance Show MyState where
  show Finish = "Finish"
  show (GotTags _ _) = "GotTags"
  show TabulaRasa = "TabulaRasa"

rbDissector ∷ NominalDiffTime → FilePath → MyState → Parser (PResult MyState SGSNBasicEntry ())
rbDissector _ _ Finish = return $ Exit ()
-- Note the 'snd' below. Basically, we discard some input.
-- TODO: We need some kind of "uncertain" dates to prevent such losses...
rbDissector _ _ TabulaRasa = Loop <$> (GotTags <$> getTags <*> (snd <$> toNextEntry))
rbDissector dt fn (GotTags tags (d, t)) = do
  txt ← (Right <$> toNextEntry) <|> (Left <$> takeWhile (const True))
  let m = BasicLogEntry {
          _basic_date = dt `addUTCTime` posixSecondsToUTCTime (fromIntegral d)
        , _basic_origin = t'' `OCons` Location { _file = fn }
        , _basic_text = either id fst txt
        }
      t'' = case M.lookup t tags of
             Nothing → "???"
             Just "undefined_tag" → "tag #" ++ show t
             Just x  → fmap w2c $ B.unpack x
      s' = case txt of
            Left _ → Finish
            Right (_, (d', t')) → GotTags tags (d', t')
  return $ Yield s' m

mergeSameOrigin ∷ (Monad m)
                ⇒ Producer SGSNBasicEntry m a
                → Producer SGSNBasicEntry m a
mergeSameOrigin p0 = loop p0 Nothing M.empty
  where
    mergeEntries (BasicLogEntry d1 o1 t1) (BasicLogEntry d2 o2 t2)
      | d1 == d2 && o1 == o2 = BasicLogEntry d1 o1 $ B.concat [t1, t2]
    loop p t m = do
      n ← lift $ next p
      case n of
        Right (e@BasicLogEntry{_basic_date=t', _basic_origin=o}, p')
          | Just t' == t → loop p' t $ M.insertWith mergeEntries o e m
          | otherwise → mapM_ yield m >> loop p' (Just t') M.empty
        Left r → do
          mapM_ yield m
          return r
