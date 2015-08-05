{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, GADTs, NoMonomorphismRestriction #-}
module Text.LogMerger.TzInfo
       (
         readTzInfo
       ) where

import Data.Time
import Data.Time.Clock.POSIX
import Text.LogMerger.Logs.Util
import Control.Applicative
import Control.Monad
import Control.Monad.Warning
import System.IO
import qualified Data.ByteString as BL
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (
    skipSpace
  , decimal
  , signed
  , isEndOfLine
  , endOfLine
  , char8)
import Prelude hiding (takeWhile)

tzParser = do
  let sp = skipSpace *> decimal <* skipSpace
  posix ←  posixSecondsToUTCTime . fromIntegral <$> decimal <?> "POSIX time"
  endOfLine
  day ← fromGregorian <$> sp <*> sp <*> sp <?> "Localtime day"
  (hh, mm, ss) ← (,,) <$> sp <*> sp <*> sp <?> "Localtime time delta"
  let local = UTCTime {
          utctDay     = day
        , utctDayTime = secondsToDiffTime $ (3600 * hh) + (mm*60) + ss
        }
  return $ local `diffUTCTime` posix
  

readTzInfo ∷ (MonadWarning [String] String m, MonadIO m, Functor m) ⇒
             String → m NominalDiffTime
readTzInfo f = do
  -- This is meh
  h ← openFile' f ReadMode
  c ← liftIO $ BL.hGetContents h
  liftIO $ hClose h
  case parseOnly tzParser c of
   Right r → return r
   Left e → throwW $ "Unable to read tzinfo: " ++ show e
