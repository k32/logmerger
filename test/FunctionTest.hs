{-# LANGUAGE OverloadedStrings, UnicodeSyntax, FlexibleInstances, FlexibleContexts #-}
import Test.Framework (defaultMain, testGroup)
import Network.VSGSN.Types
import Network.VSGSN.Logs.Types
import Network.VSGSN.Logs.Isp as Isp
import Control.Applicative
import Pipes
import qualified Pipes.ByteString as P
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.ByteString as B
import Data.String
import Control.Monad.Identity
import Data.Time

{- Mock Logs -}
class LogEntry a where
  basicLogEntry ∷ a → SGSNBasicEntry

newtype IspLogEntry = IspLogEntry { unIsp ∷ SGSNBasicEntry }
  deriving (Eq, Show)
  
instance LogEntry IspLogEntry where
  basicLogEntry = unIsp

instance Arbitrary IspLogEntry where
  arbitrary = do
    date ← fromGregorian <$> choose (0, 3000) <*> choose (1, 12) <*> choose (1, 31)
    time ← toEnum <$> choose (0, 86401)
    let origin = Location "TestPath"
    txt ← fromString <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [';', '-'])
    return (IspLogEntry $ BasicLogEntry {
        _basic_date = UTCTime date (secondsToDiffTime time)
      , _basic_origin = origin
      , _basic_text = txt
      })

{- Producers -}
pprintIsp ∷ (Monad m) 
          ⇒ String 
          → [IspLogEntry] 
          → Producer B.ByteString m ()
pprintIsp path l = do
    yield header 
    mapM_ (f . unIsp) l
  where
    header = "Content of isp.log\n==================\n"
    yield' = yield . fromString
    
    f BasicLogEntry {
        _basic_date = d
      , _basic_origin = o
      , _basic_text = t
      } = do
         yield' $ formatTime defaultTimeLocale "%F %T UTC%z;" d
         yield t
         yield "\n"

{- Utils -}
sink ∷ (Monad m) 
     ⇒ Producer a m r 
     → m [a]
sink p = runEffect $ loop [] p
  where loop l p = do
          a ← lift $ next p
          case a of
            Left _ → return l
            Right (a', p') → loop (l++[a']) p'

parserTest ∷ (LogEntry e) 
           ⇒ (String → [e] → Producer B.ByteString Identity a)
           → String
           → LogFormat Identity a
           → [e]
           → Property
parserTest pprinter path lf l =
  let parser = (_dissector lf) (fromIntegral 0) path
      pprinter' = pprinter path l
      pprinted = B.concat . runIdentity $ sink pprinter'
      l' = map basicLogEntry l
      infix 4 =^^=
      a =^^= b = counterexample (show a ++ "/=\n" ++ show b ++";\nPRETTY:\n" ++ show pprinted) (a==b)
  in 
    l' =^^= runIdentity (sink $ parser pprinter')

{- Tests -}
prop_ispLogParse l = parserTest pprintIsp "TestPath" Isp.logFormat l

parsing = testGroup "Log parsing" [
            testProperty "isp.log parsing" prop_ispLogParse
          ]

main = defaultMain [
           parsing
        ]