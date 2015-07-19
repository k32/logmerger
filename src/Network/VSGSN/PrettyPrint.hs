{-# LANGUAGE UnicodeSyntax, OverloadedStrings, LambdaCase, FlexibleContexts #-}
module Network.VSGSN.PrettyPrint (
    defaultHeaderFormat
  , defaultLineFormat
  , makeLogEntryPP
  ) where

import Pipes
import Pipes.ByteString as P
import Text.Format
import Control.Monad.Warning
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Network.VSGSN.Types
import Network.VSGSN.Logs.Types

defaultHeaderFormat, defaultLineFormat ∷ B.ByteString
defaultHeaderFormat = "~~~ ${YY}-${MM}-${DD} ${hh}:${mm}:${ss} (${origin}) ~~~\n"
defaultLineFormat = "$mm:$ss> $l"

makeLogEntryPP ∷ (Monad m2, MonadWarning [String] e m) 
               ⇒ B.ByteString
               → B.ByteString
               → m (Pipe SGSNBasicEntry B.ByteString m2 ())
makeLogEntryPP hf lf = do
  hf' ← case makeFormat hf of
          Right x → return x
          Left s → {- warning ["Wrong header format"] >> -} return . either undefined id $ makeFormat defaultHeaderFormat
  lf' ← case makeFormat lf of
          Right x → return x
          Left s → {- warning ["Wrong line format"] >> -} return . either undefined id $ makeFormat defaultLineFormat
  return $ printLogEntry hf' lf'

printLogEntry ∷ Monad m ⇒ Format → Format → Pipe SGSNBasicEntry B.ByteString m ()
printLogEntry hf lf = forever $ do
  BasicLogEntry {
      _basic_origin = origin
    , _basic_date = UTCTime {
          utctDay = date
        , utctDayTime = time
        }
    , _basic_text = txt
    } ← await
  let (year, month, day) = toGregorian date
      format' a b = P.fromLazy . toLazyByteString $ format a b -- TODO: Is it effective?
      p0 x | x<10 = char8 '0' <> int8Dec x
           | True = int8Dec x
      f = \case
            "YY" → int32Dec $ fromIntegral year
            "MM" → p0 $ fromIntegral month
            "DD" → p0 $ fromIntegral day
            --"hh" → p0 . fromIntegral $ 
            a → byteString a
      lb = format lf f
      --txt' = {- B.intercalate lb $-} B.split '\n' txt
  format' hf f
  yield txt
  yield "\n"