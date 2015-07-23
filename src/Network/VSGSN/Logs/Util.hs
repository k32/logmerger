{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax, OverloadedStrings,
             FlexibleContexts, TupleSections, GADTs #-}

module Network.VSGSN.Logs.Util (
   skipAnyLine
 , openFile'
 , parse'
 , follow
 , PResult(..)
 , (<!)
 ) where

import Pipes
import qualified Pipes.ByteString as P
import Network.VSGSN.Types
import Network.VSGSN.Logs.Types
import Data.Attoparsec.ByteString hiding (try)
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, endOfLine, anyChar)
import qualified Data.ByteString as B
import Control.Applicative
import Debug.Trace
import Control.Monad.Warning
import System.IO (IOMode(..), openFile, Handle)
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.IORef

type Fin = IORef [IO ()]

follow ∷ MonadIO m ⇒ Int → m () → m r
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

-- | Raise an exception when monadic function (typically a pipe) exits
(<!) ∷ (MonadTrans t, Monad (t m), MonadWarning w e m) ⇒ e → t m a → t m b
e <! p = p >> lift (throwW e)

-- Attoparsec isn't a monad transformer, so we can't simply shift control
-- over pipe to a parser...
data PResult s o r = Yield s o | Loop s | Exit r
instance Show (PResult s o r) where
  show (Yield _ _) = "Yield"
  show (Loop _) = "Loop"
  show (Exit _) = "Exit"

parse' ∷ (Monad m)
       ⇒ (s → Parser (PResult s b r)) -- ^ Attoparsec parser
       → s                            -- ^ Initial state of the parser
       → Pipe P.ByteString b m (Either [String] r)
parse' p s0 = loop . parse (p s0) =<< await
  where loop i =
          case i of
           Partial p' → loop . p' =<< await
           f@(Fail _ e1 e2) → return $ Left ["Parsing error: " ++ (show e1) ++ " in " ++ (show e2)]
           Done i r → case r of
                       Yield s' o → do
                         yield o
                         loop $ parse (p s') i
                       Loop s' → loop $ parse (p s') i
                       Exit r  → return $ Right r

skipAnyLine ∷ Parser ()
skipAnyLine = takeTill isEndOfLine >> endOfLine
