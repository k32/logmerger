{-# LANGUAGE UnicodeSyntax, OverloadedStrings, LambdaCase, FlexibleContexts,
             NoMonomorphismRestriction #-}
module Network.VSGSN.PrettyPrint (
    defaultHeaderFormat
  , defaultLineFormat
  , makeLogEntryPP
  ) where
    
import Data.String
import Data.Fixed
import Pipes
import Pipes.ByteString as P
import Text.Format
import Control.Monad.Warning
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Network.VSGSN.Types
import Network.VSGSN.Logs.Types

defaultHeaderFormat, defaultLineFormat ∷ String
defaultHeaderFormat = "\n~~~ ${YY}-${MM}-${DD} ${hh}:${mm}:${ss} (${o}) ~~~"
defaultLineFormat = "\n|${hh}:${mm}:${ss}| "

makeLogEntryPP ∷ (Monad m2, MonadWarning [String] e m) 
               ⇒ String
               → String
               → m (Pipe SGSNBasicEntry B.ByteString m2 ())
makeLogEntryPP hf lf = do
  let fromEither = return . either (error "ERROR. Default log format is broken.") id
  hf' ← case makeFormat hf of
          Right x → return x
          Left s → warning ["Wrong header format"] >> 
                   fromEither (makeFormat defaultHeaderFormat)
  lf' ← case makeFormat lf of
          Right x → return x
          Left s → warning ["Wrong line format"] >> 
                   fromEither (makeFormat defaultLineFormat)
  return $ printLogEntry hf' lf'

printLogEntry ∷ Monad m 
              ⇒ Format 
              → Format 
              → Pipe SGSNBasicEntry B.ByteString m ()
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
      TimeOfDay hour minute sec = timeToTimeOfDay time
      format' a b = P.fromLazy . toLazyByteString $ format a b -- TODO: Is it effective?
      p0 s x | x<10 = char8 '0' <> s x
             | True = s x
      i8f = int8Dec . fromIntegral
      f = \case
            "YY" → int32Dec $ fromIntegral year
            "MM" → p0 i8f month
            "DD" → p0 i8f day
            "hh" → p0 i8f hour
            "mm" → p0 i8f minute
            "ss" → p0 (fromString . showFixed True) sec
            "o" → byteString . fromString $ show origin
            a → byteString a
      lb = toLazyByteString $ format lf f
      newline = fromIntegral $ fromEnum '\n'
      txt' = B.split newline txt
  format' hf f
  forM_ txt' $ \i → do
    P.fromLazy lb
    yield i