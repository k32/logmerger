{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, GADTs #-}

module Network.VSGSN.Logs.Util (
   skipAnyLine
 , openFile'
 , follow
 , (<!)
 , yymmdd
 , hhmmss
 ) where

import Pipes
import Pipes.Dissect
import qualified Pipes.ByteString as P
import Network.VSGSN.Types
import Network.VSGSN.Logs.Types
import Data.Attoparsec.ByteString hiding (try)
import Data.Attoparsec.ByteString.Char8 (
    isEndOfLine
  , endOfLine
  , anyChar
  , decimal
  , skipSpace
  , Parser
  )
import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad.Warning
import System.IO (IOMode(..), openFile, Handle)
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.List (intercalate)
import Data.Time (
    fromGregorian
  , secondsToDiffTime
  , Day
  )

yymmdd ∷ Parser Day
yymmdd = fromGregorian <$> decimal <*> ("-" *> decimal) <*> ("-" *> decimal) <* skipSpace <?> "date"

hhmmss ∷ Parser DiffTime
hhmmss = do
  hh ← decimal <* ":" <?> "hours"
  mm ← decimal <* ":" <?> "minutes"
  ss ← decimal <?> "seconds"
  return $ secondsToDiffTime $ (hh * 3600) + (mm * 60) + ss

type Fin = IORef [IO ()]

follow ∷ MonadIO m
       ⇒ Int 
       → m ()
       → m r
follow n m = forever $ do
                m
                liftIO $ threadDelay n

-- | Open handle in Warning monad
openFile' ∷ (MonadWarning [String] String m, MonadIO m) 
          ⇒ FilePath 
          → IOMode 
          → m Handle
openFile' fn fm = do
  r ← liftIO $ try $ openFile fn fm
  case r of
   Left e → throwW $ show (e ∷ SomeException)
   Right h → return h

-- | Raise an exception when a monadic function (typically a pipe) exits
(<!) ∷ (MonadTrans t, Monad (t m), MonadWarning w e m) 
     ⇒ e 
     → t m a 
     → t m b
e <! p = p >> lift (throwW e)

skipAnyLine ∷ Parser ()
skipAnyLine = takeTill isEndOfLine >> endOfLine
