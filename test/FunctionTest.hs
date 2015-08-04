{-# LANGUAGE OverloadedStrings, UnicodeSyntax, FlexibleInstances, FlexibleContexts, LambdaCase #-}
import Test.Framework (defaultMain, testGroup)
import Network.VSGSN.Types
import Network.VSGSN.Logs.Types
--import Network.VSGSN.Logs.Isp as Isp
import Network.VSGSN.Logs.LinuxRB as RB
import Control.Applicative
import Pipes
import Pipes.Interleave
import Network.VSGSN.MergeSame
import qualified Pipes.ByteString as P
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.ByteString as B
import Text.Printf
import Data.String
import Data.Time.Clock.POSIX
import Data.List (sort)
import Data.Function (on)
import Control.Monad.Identity
import Data.Time
import Data.Word (Word8)
import Text.Format
import qualified Data.Map as M
import Data.Monoid

{- Utils -}
byteStringToString ∷ B.ByteString 
                   → String
byteStringToString = map (toEnum . fromIntegral) . B.unpack

newtype EasyMode = EasyMode { toString ∷ String }
  deriving (Show)

instance Arbitrary EasyMode where
  arbitrary = EasyMode <$> listOf (oneof [
      choose ('a','z')
    , choose ('A', 'Z')
    , choose ('0', '9')
    , elements "-_:"])

instance Arbitrary UTCTime where
  arbitrary = do
    date ← fromGregorian <$> choose (1970, 3000) <*> choose (1, 12) <*> choose (1, 31)
    time ← toEnum <$> choose (0, 0) -- (0, 86400)
    return $ UTCTime date $ secondsToDiffTime time

isSorted ∷ (Ord a) 
         ⇒ [a] 
         → Bool
isSorted x = all (uncurry (<=)) $ zip x $ tail x
        
sink ∷ Producer a Identity r 
     → (r, [a])
sink p = runIdentity . runEffect $ loop [] p
  where loop l p = do
          a ← lift $ next p
          case a of
            Left r → return (r, l)
            Right (a', p') → loop (l++[a']) p'

-- | Split a bytestring to chunks of fixed size to test parsing
chunks ∷ Word
       → B.ByteString
       → Producer B.ByteString Identity x
chunks c s = go s
  where go s | B.null s = return undefined
             | True = do
                 let (y, s') = B.splitAt (fromIntegral c) s
                 yield y
                 go s'

okParser ∷ (Show a, Eq a, Show r)
         ⇒ B.ByteString
         → Word
         → (Producer B.ByteString Identity x → Producer a Identity r)
         → [a]
         → Property
okParser p c d b = c > 0 ==> counterexample msg (a==b)
  where (r, a) = sink $ d (chunks c p)
        msg = show b ++ "/=" ++ show a ++"\n<PRETTY>\n" ++ byteStringToString p ++"\n</PRETTY>\nPARSER RETURNED: "++show r

{- Tests -}
prop_RibgbufferParse ∷ (Int, Word, String, [EasyMode], [(UTCTime, Word8, String)]) 
                     → Property
prop_RibgbufferParse (ltOffset, chunkSize, remains, tags0, l) = 
  let
    tags = map toString tags0
    n_tags = length tags
    entries = [BasicLogEntry {
                 _basic_date = d
               , _basic_origin = tags !! t' `OCons` Location "TestFile"
               , _basic_text = fromString txt
               } | (d, t, txt) ← l, let t' = (fromIntegral t) `rem` n_tags]
    tags' = M.fromList $ zip tags [0 ∷ Int ..]
    formatEntry BasicLogEntry{
                    _basic_date = d
                  , _basic_text = txt
                  , _basic_origin = o
                  } = printf "~RB03~[%08X %02X]%s" (d' ∷ Int) o' txt'
      where d' = (round $ utcTimeToPOSIXSeconds d) - ltOffset
            o' = tags' M.! t
            t `OCons` _ = o
            txt' = byteStringToString txt
    pprint = fromString $ concat [
        (printf "RB03\nDump of %d tags\nHEX String\n--- --------------------------------------------------------------\n" n_tags)
      , (M.toList tags') >>= (\(n, m) → printf " %02X %s\n" m n)
      , remains
      , entries >>= formatEntry
      ]
    parser = (_dissector RB.logFormat) (fromIntegral ltOffset) "TestFile"
  in
    (n_tags > 0 && n_tags < 256) ==>
    (all (not . null) tags) ==>
    okParser pprint chunkSize parser entries
  
--prop_ispLogParse l = parserTest pprintIsp "TestPath" Isp.logFormat l

prop_nEntitiesConservation ∷ [[Int]] 
                           → Property
prop_nEntitiesConservation l = n === n'
  where n = sum $ map length l
        n' = length l'
        (_, l') = sink $ interleave s
        s = map each l

prop_interleaveSorting ∷ [(String, [(UTCTime, String)])] 
                       → Bool
prop_interleaveSorting l = isSorted l'
  where (_, l') = sink $ interleave $ map mkLogEntries l
        mkLogEntries (o, s) = each $ sort [BasicLogEntry {
                                             _basic_date = d
                                           , _basic_origin = Location o
                                           , _basic_text = fromString t
                                           } | (d, t) ← s] ∷ Producer SGSNBasicEntry Identity ()

prop_mergeSameContentConservation ∷ [(String, UTCTime, [String])] 
                                  → Property
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
        (_, e') = sink $ mergeSameOrigin mkLogEntries
        eq (a, b) = (_basic_origin a) == (_basic_origin b) &&
                    (_basic_date a) == (_basic_date b) &&
                    (B.length $ _basic_text a) == (B.length $ _basic_text b)
  -- There's a little probability that 'Arbitrary' generates same date and origin,
  -- so the testcase is potentially unstable.
  -- But I neglect it for now

merging = testGroup "Log merging" [
            testProperty "interleave: Conservation of the number of entities" prop_nEntitiesConservation
          , testProperty "mergeSameOrigin: Conservation of content length" prop_mergeSameContentConservation
          , testProperty "interleave: Ordering" prop_interleaveSorting
          ]

parsing = testGroup "Log parsing" [
            --testProperty "isp.log parsing" prop_ispLogParse
            testProperty "Linux ringbuffer parsing" prop_RibgbufferParse
          ]

main = defaultMain [
          parsing
        , merging
        ]