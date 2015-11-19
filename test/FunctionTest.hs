{-# LANGUAGE OverloadedStrings, UnicodeSyntax, FlexibleInstances, FlexibleContexts, LambdaCase #-}
import Test.Framework (defaultMain, testGroup)
import Text.LogMerger.Types
import Text.LogMerger.Logs.Types
import Text.LogMerger.Logs.Isp as ISP
import Text.LogMerger.Logs.CLI as CLI
import Text.LogMerger.Logs.LinuxRB as RB
import Control.Applicative
import Pipes
import Pipes.Interleave
import Text.LogMerger.MergeSame
import qualified Pipes.ByteString as P
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.ByteString as B
import Text.Printf
import Data.String
import Data.Time.Clock.POSIX
import Data.List (nub, sort)
import Data.Function (on)
import Control.Monad.Identity
import Data.Time
import Data.Word (Word8)
import Text.Format
import qualified Data.Map as M
import Data.Monoid
import Control.Lens (
    (&)
  , (%~)
  , view
  , over
  , _1, _2
  , views
  , sets
  , mapped
  )

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
    , elements "-:!@#$%^&*()_+"])
    
instance Arbitrary SGSNBasicEntry where
  arbitrary = do
    t ← arbitrary
    txt ← arbitrary
    return BasicLogEntry {
        _basic_date = t
      , _basic_origin = [] -- [Location "TestFile"]
      , _basic_text = fromString txt
      }

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
okParser p c d b = (c > 0) ==> counterexample msg (a==b)
  where (r, a) = sink $ d (chunks c p)
        msg = show b ++ "/=" ++ show a ++"\n<PRETTY>\n" ++ byteStringToString p ++"\n</PRETTY>\nPARSER RETURNED: "++show r

isOneLiner ∷ SGSNBasicEntry → Bool
isOneLiner = B.notElem (fromIntegral $ fromEnum '\n') . _basic_text

{- Tests -}
prop_CLILogParse ∷ (Word, Int, [SGSNBasicEntry])
                 → Property
prop_CLILogParse (chunkSize, ltOffset, l) = 
  let
    printEntry BasicLogEntry {
        _basic_date = d
      , _basic_text = t
      } = concat [formatTime defaultTimeLocale "%F %T" d, ",", byteStringToString t, "\n"] 
    pprint = fromString $ l >>= printEntry
    parser = _dissector CLI.logFormat
  in
    all isOneLiner l ==> okParser pprint chunkSize parser l

prop_IspLogParse ∷ (Word, Int, [SGSNBasicEntry])
                 → Property
prop_IspLogParse (chunkSize, ltOffset, l) = 
  let
    printEntry BasicLogEntry {
        _basic_date = d
      , _basic_text = t
      } = concat [formatTime defaultTimeLocale "%F %T UTC%z" d, ";", byteStringToString t, "\n"] 
    pprint = fromString $ concat [
        "Content of isp.log\n==================\n"
      , l >>= printEntry
      ]
    parser = _dissector ISP.logFormat
  in
    all isOneLiner l ==> okParser pprint chunkSize parser l

prop_RibgbufferParse ∷ (Int, Word, String, [EasyMode], [(Word8, SGSNBasicEntry)]) 
                     → Property
prop_RibgbufferParse (ltOffset, chunkSize, remains, tags0, l) = 
  let
    tags = map toString tags0
    n_tags = length tags
    appendTag t o = OData (tags !! t') : o
      where t' = (fromIntegral t) `rem` n_tags
    entries = map (\(t, e) → e & basic_origin %~ appendTag t) l
    tags' = M.fromList $ zip tags [0 ∷ Int ..]
    formatEntry BasicLogEntry{
                    _basic_date = d
                  , _basic_text = txt
                  , _basic_origin = o
                  } = printf "~RB03~[%08X %02X]%s" (d' ∷ Int) o' txt'
      where d' = (round $ utcTimeToPOSIXSeconds d)
            o' = tags' M.! t
            (OData t): _ = o
            txt' = byteStringToString txt
    pprint = fromString $ concat [
        (printf "RB03\nDump of %d tags\nHEX String\n--- --------------------------------------------------------------\n" n_tags)
      , (M.toList tags') >>= (\(n, m) → printf " %02X %s\n" m n)
      , remains
      , entries >>= formatEntry
      ]
    parser = _dissector RB.logFormat
  in
    (n_tags > 0 && n_tags < 256) && (length (nub tags) == length tags) && (all (not . null) tags) ==>
    okParser pprint chunkSize parser entries
  
--prop_ispLogParse l = parserTest pprintIsp "TestPath" Isp.logFormat l

prop_nEntitiesConservation ∷ [[Int]] 
                           → Property
prop_nEntitiesConservation l = n === n'
  where n = sum $ map length l
        n' = length l'
        (_, l') = sink $ interleave s
        s = map each l

prop_interleaveSorting ∷ [(String, [SGSNBasicEntry])] 
                       → Bool
prop_interleaveSorting l = isSorted l'
  where (_, l') = sink $ interleave $ map mkLogEntries l
        mkLogEntries (o, s) = each $ sort s

prop_mergeSameContentConservation ∷ [(String, UTCTime, [String])] 
                                  → Property
prop_mergeSameContentConservation l = (length e == length e') .&&. all eq (zip e e')
  where mkLogEntries = each [BasicLogEntry {
                               _basic_date = d
                             , _basic_origin = [Location o]
                             , _basic_text = fromString i
                             } | (o, d, s) ← l, i ← s]
        e = [BasicLogEntry {
               _basic_date = d
             , _basic_origin = [Location o]
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
            testProperty "Basic isp.log parsing" prop_IspLogParse
          , testProperty "Basic Linux ringbuffer parsing" prop_RibgbufferParse
          , testProperty "Basic CLI.log parsing" prop_CLILogParse
          ]

main = defaultMain [
          parsing
        , merging
        ]
