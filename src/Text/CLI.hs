-- Very stupid and simple CLI parser.
{-# LANGUAGE UnicodeSyntax, Rank2Types, MultiParamTypeClasses, 
             GADTs, KindSignatures, FlexibleInstances, TemplateHaskell #-}
module Text.CLI (
    parseCli
  , MyS(..)
  , CliDescr(..)
  , CliParam(..)
  , CliSetter(..)
  , setter
  , (.→)
  ) where

import System.Environment
import Control.Monad
import Data.Maybe (catMaybes)
import Data.List
import qualified Data.Set as S
import Control.Lens
import Control.Lens.TH
import Data.Monoid

-- class CliParamSetter a where
--   cliParamSetter ∷ a s → MySetter 


-- instance CliParamSetter MySetter where
--   cliParamSetter = id

-- instance (a s ~ ASetter' s String) ⇒ CliParamSetter a where
--   cliParamSetter s = s :$ id
 
data CliSetter ∷ * → * where
  CliParameter ∷ {
    _cliParSetter ∷ MyS a
  , _cliParOperand ∷ String
  } → CliSetter a
  CliFlag ∷ {
    _cliFlagSetter ∷ ASetter' a Bool
  } → CliSetter a

data MyS ∷ * → * where
  (:$) ∷ {
     _mysetter ∷ ASetter' s a
   , _myparser ∷ (String → a)
   } → MyS s

(.→) ∷ CliSetter a → ASetter' b a → CliSetter b
a@CliParameter{_cliParSetter = sa :$ sb} .→ b =
   a{
     _cliParSetter = (b . sa) :$ sb
   }
CliFlag{_cliFlagSetter = a} .→ b = CliFlag{_cliFlagSetter = b . a}

-- | Structure representing a CLI parameter
data CliParam ∷ * → * where
    CliParam ∷ {
      _long ∷ String            -- ^ Long name of a parameter
    , _short ∷ Maybe Char       -- ^ Shortcut for the parameter
    , _descr ∷ String           -- ^ Parameter description
    , _setter ∷ CliSetter a     -- ^ Setter
    } → CliParam a
makeLenses ''CliParam

-- | Structure containing all parameters accepted by an application
data CliDescr a b =
    CliDescr {
      _globalAttrs ∷ [CliParam (a b)] -- ^ Global CLI attributes
    , _perFileAttrs ∷ [CliParam b]    -- ^ Per-file CLI attributes
    }

instance Monoid (CliDescr a b) where
  mempty = CliDescr {
      _globalAttrs = []
    , _perFileAttrs = []
    }
  
  mappend a b = CliDescr {
      _globalAttrs = _globalAttrs a `mappend` _globalAttrs b
    , _perFileAttrs = _perFileAttrs a `mappend` _perFileAttrs b
    }

-- cliParameter ∷ (CliParamSetter s)
--              ⇒ s 
--              → String
--              → CliSetter a
-- cliParameter a = CliParameter (cliParamSetter a)
  
-- | Expand shortcuts
expandCli ∷ [String] → [String]
expandCli = (>>= f)
  where f ('-':t)
          | head t /= '-' = g t
        f x = [x]

        g [] = []
        g ('n':f:t) = ("-n" ++ [f]):g t
        g (f:t) = ('-':[f]):g t

checkDescr ∷ CliDescr a b → S.Set Char
checkDescr CliDescr{_globalAttrs = g, _perFileAttrs = p} = 
    checkN g `seq` checkN p `seq` checkDups g `seq` checkDups p 
  where
    checkN = foldl' isN () . catMaybes . map _short
    isN _ a | a == 'n' = error "Using 'n' is not allowed for short flags."
            | True = ()
            
    checkDups l = foldl' dup S.empty (map _long l) `seq`
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
match (CliParam{_short = s, _long = l, _setter = (CliParameter (t:$g) _)}:_) a (f:v:r) 
  | flagEq True f s l = Just (set t (g v) a, r)
match (CliParam{_short = s, _long = l, _setter = CliFlag t}:_) a (f:r) 
  | flagEq True f s l = Just (set t True a, r)
  | flagEq False f s l = Just (set t False a, r)
match (_:p) a r = match p a r 
match [] a r = Nothing

matches ∷ [CliParam a] → (a, [String]) → (a, [String])
matches p (a, l) = case match p a l of
                     Just (a', l') → matches p (a', l')
                     Nothing → (a, l)

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
  , "PER-FILE_FLAGS:"
  , _perFileAttrs descr >>= par
  , postamble
  ]
  where
    par CliParam{_descr = d, _short = s, _long = l, _setter = CliParameter _ t} = 
      "  --" ++ l ++ short [] s ++ " " ++ t ++ " : " ++ d ++ "\n"
    par CliParam{_descr = d, _short = s, _long = l, _setter = CliFlag _} = 
      "  --" ++ l ++ short [] s ++ " : " ++ d ++ "\n" ++
      "  --no-" ++ l ++ short "n" s ++ " : Opposite of --" ++ l++"\n"
      
    short _ Nothing = ""
    short n (Just c) = " (-" ++ n ++ [c, ')']
