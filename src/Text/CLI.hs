{-# LANGUAGE UnicodeSyntax, Rank2Types, MultiParamTypeClasses, GADTs, KindSignatures #-}
module Text.CLI (parseCli, CliDescr(..), CliParam(..)) where

import System.Environment
import Control.Monad
import Data.Maybe (catMaybes)
import Data.List
import Debug.Trace
import qualified Data.Set as S

data CliParam ∷ * → * where
    CliParam ∷ {
      _long ∷ String
    , _short ∷ Maybe Char
    , _descr ∷ String
    , _setter ∷ Either (String → a → a, String) (Bool → a → a)
    } → CliParam a

data CliDescr a b =
    CliDescr {
      _globalAttrs ∷ [CliParam (a b)]
    , _perFileAttrs ∷ [CliParam b]
    }

expandCli ∷ [String] → [String]
expandCli = (>>= f)
  where f x@(h:t) = case h of
                      '-' | head t /= '-' → g t
                      _ → [x]
        g [] = []
        g ('n':f:t) = ("-n" ++ [f]):g t
        g (f:t) = ('-':[f]):g t

checkDescr ∷ CliDescr a b → S.Set Char
checkDescr CliDescr{_globalAttrs = g, _perFileAttrs = p} = 
    checkN g `seq` checkN p `seq` checkDubs g `seq` checkDubs p 
  where
    checkN = foldl' isN () . catMaybes . map _short
    isN _ a | a == 'n' = error "Using 'n' is not allowed for short flags."
            | True = ()
            
    checkDubs l = foldl' dup S.empty (map _long l) `seq`
              foldl' dup S.empty (catMaybes $ map _short l)
    dup ∷ (Show a, Ord a) ⇒ S.Set a → a → S.Set a
    dup s a | S.member a s = error $ "Duplicate flag: " ++ show a
            | True = S.insert a s

expandShort ∷ Bool → Maybe Char → Maybe String
expandShort n = fmap ((r ++) . return) 
  where r = if n
              then "-"
              else "-n"

expandLong ∷ Bool → String → Maybe String
expandLong n = Just . (r ++)
  where r = if n
              then "--"
              else "--no-"
flagEq n f s l = (elem f) $ catMaybes [expandLong n l, expandShort n s] 

match ∷ [CliParam a] → a → [String] → Maybe (a, [String])
match (CliParam{_short = s, _long = l, _setter = Left (t, _)}:_) a (f:v:r) 
  | flagEq True f s l = Just (t v a, r)
match (CliParam{_short = s, _long = l, _setter = Right t}:_) a (f:r) 
  | flagEq True f s l = Just (t True a, r)
  | flagEq False f s l = Just (t False a, r)
match (_:p) a r = match p a r 
match [] a r = Nothing

matches ∷ [CliParam a] → (a, [String]) → (a, [String])
matches p (a, l) = case match p a l of
                     Just (a', l') → matches p (a', l')
                     Nothing → (a, l)

-- Very stupid and simple CLI parser
parseCli ∷ String -- ^ Help message peamble
         → String -- ^ Help message postamble
         → a b    -- ^ Global defaults
         → (String → a b → b) -- ^ Make new element
         → (b → a b → a b) -- ^ Stuff new element into global config
         → CliDescr a b 
         → IO (a b)
parseCli preamble postamble dA dB stuff descr = 
  checkDescr descr `seq` do
    name ← getProgName
    l ← expandCli `fmap` getArgs
    when ((`elem` ["--help", "-h"]) `any` l) $
      error $ getHelp name preamble postamble descr
    let CliDescr {_globalAttrs = gp, _perFileAttrs = pp} = descr
        (cA, l') = matches gp (dA, l)
        
        files (a, []) = a
        files (a, (fn:rest)) = let
            (b, rest') = matches pp (dB fn cA, rest)
          in
            files (stuff b a, rest')
    return $ files (cA, l')
  
getHelp ∷ String → String → String → CliDescr a b → String
getHelp name summary postamble descr = unlines [
    ""
  , summary
  , ""
  , "USAGE: "++name++" GLOBAL_FLAGS (FILE PER-FILE_FLAGS)*"
  , "   where"
  , ""
  , "GLOBAL_FLAGS:"
  , _globalAttrs descr >>= par
  , ""
  , "PER-FILE FLAGS:"
  , _perFileAttrs descr >>= par
  , postamble
  ]
  where
    par CliParam{_descr = d, _short = s, _long = l, _setter = Left (_, t)} = 
      "  --" ++ l ++ short [] s ++ " " ++ t ++ " : " ++ d ++ "\n"
    par CliParam{_descr = d, _short = s, _long = l, _setter = Right _} = 
      "  --" ++ l ++ short [] s ++ " : " ++ d ++ "\n" ++
      "  --no-" ++ l ++ short "n" s ++ " : Opposite of " ++ l++"\n"
      
    short _ Nothing = ""
    short n (Just c) = " (-" ++ n ++ [c, ')']
