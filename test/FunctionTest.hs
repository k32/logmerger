{-# LANGUAGE OverloadedStrings, UnicodeSyntax, FlexibleInstances, FlexibleContexts #-}
import Test.Framework (defaultMain, testGroup)
import Network.VSGSN.Types
import Network.VSGSN.Logs.Types
import Network.VSGSN.Logs.Isp as Isp
import Control.Applicative
import Pipes
import Pipes.Interleave
import Network.VSGSN.MergeSame
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

instance Arbitrary UTCTime where
  arbitrary = do
    date ← fromGregorian <$> choose (0, 3000) <*> choose (1, 12) <*> choose (1, 31)
    time ← toEnum <$> choose (0, 86401)
    return $ UTCTime date $ secondsToDiffTime time

instance Arbitrary IspLogEntry where
  arbitrary = do
    date ← arbitrary
    let origin = Location "TestPath"
    txt ← fromString <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [';', '-'])
    return (IspLogEntry $ BasicLogEntry {
        _basic_date = date
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
sink ∷ Producer a Identity r 
     → [a]
sink p = runIdentity . runEffect $ loop [] p
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
      pprinted = B.concat $ sink pprinter'
      l' = map basicLogEntry l
      infix 4 =^^=
      a =^^= b = counterexample (show a ++ "/=\n" ++ show b ++";\nPRETTY:\n" ++ show pprinted) (a==b)
  in 
    l' =^^= (sink $ parser pprinter')

{- Tests -}
prop_ispLogParse l = parserTest pprintIsp "TestPath" Isp.logFormat l

prop_nEntitiesConservation ∷ [[Int]] → Bool
prop_nEntitiesConservation l = n == n'
  where n = sum $ map length l
        n' = length $ sink $ interleave s
        s = map each l
        
prop_mergeSameContentConservation ∷ [(String, UTCTime, [String])] → Property
prop_mergeSameContentConservation l = (length e == length e') .&&. all eq (zip e e')
  where mkLogEntries = each [BasicLogEntry {
                               _basic_date = d
                             , _basic_origin = Location o
                             , _basic_text = fromString i
                             } | (o, d, s) ← l, i ← s]
        e = [BasicLogEntry {
               _basic_date = d
             , _basic_origin = Location o
             , _basic_text = fromString $ concat s
             } | (o, d, s) ← l, not (null s)]
        e' = sink $ mergeSameOrigin mkLogEntries
        eq (a, b) = (_basic_origin a) == (_basic_origin b) &&
                    (_basic_date a) == (_basic_date b) &&
                    (B.length $ _basic_text a) == (B.length $ _basic_text b)
  -- There's a little probability that 'Arbitrary' generates same date and origin,
  -- so the testcase is potentially unstable.
  -- But I neglect it for now
            

merging = testGroup "Log merging" [
            testProperty "Conservation of the number of entities" prop_nEntitiesConservation
          , testProperty "mergeSameOrigin" prop_mergeSameContentConservation
          ]

parsing = testGroup "Log parsing" [
            testProperty "isp.log parsing" prop_ispLogParse
          ]

main = defaultMain [
          merging
           --parsing
        ]