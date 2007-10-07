{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS -fvia-C #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
-- 6.4 gives a name shadow warning I haven't tracked down.

--
-- | This marvellous module contributed by Thomas J\344ger
--
module Plugin.Pl.Rules (RewriteRule(..), rules, fire) where

import Plugin.Pl.Common

import Data.Array
import qualified Data.Set as S

import Control.Monad.Fix (fix)

--import PlModule.PrettyPrinter

-- Next time I do somthing like this, I'll actually think about the combinator
-- language before, instead of producing something ad-hoc like this:
data RewriteRule 
  = RR Rewrite Rewrite
  | CRR (Expr -> Maybe Expr)
  | Down RewriteRule RewriteRule
  | Up RewriteRule RewriteRule
  | Or [RewriteRule]
  | OrElse RewriteRule RewriteRule
  | Then RewriteRule RewriteRule
  | Opt RewriteRule
  | If RewriteRule RewriteRule
  | Hard RewriteRule

-- No MLambda here because we only consider closed Terms (no alpha-renaming!).
data MExpr
  = MApp !MExpr !MExpr
  | Hole !Int
  | Quote !Expr
  deriving Eq

--instance Show MExpr where
--  show = show . fromMExpr

data Rewrite = Rewrite {
  holes :: MExpr,
  rid :: Int -- rlength - 1
} --deriving Show

-- What are you gonna do when no recursive modules are possible?
class RewriteC a where
  getRewrite :: a -> Rewrite 

instance RewriteC MExpr where
  getRewrite rule = Rewrite {
    holes   = rule,
    rid = 0
  }

type ExprArr = Array Int Expr

myFire :: ExprArr -> MExpr -> MExpr
myFire xs (MApp e1 e2) = MApp (myFire xs e1) (myFire xs e2)
myFire xs (Hole h) = Quote $ xs ! h
myFire _ me = me

nub' :: Ord a => [a] -> [a]
nub' = S.toList . S.fromList

uniqueArray :: Ord v => Int -> [(Int, v)] -> Maybe (Array Int v)
uniqueArray n lst 
  | length (nub' lst) == n = Just $ array (0,n-1) lst
  | otherwise = Nothing              

match :: Rewrite -> Expr -> Maybe ExprArr
match (Rewrite hl rid') e  = uniqueArray rid' =<< matchWith hl e

fire' :: Rewrite -> ExprArr -> MExpr
fire' (Rewrite hl _)   = (`myFire` hl)

fire :: Rewrite -> Rewrite -> Expr -> Maybe Expr
fire r1 r2 e = (fromMExpr . fire' r2) `fmap` match r1 e

matchWith :: MExpr -> Expr -> Maybe [(Int, Expr)]
matchWith (MApp e1 e2) (App e1' e2') = 
  liftM2 (++) (matchWith e1 e1') (matchWith e2 e2')
matchWith (Quote e) e' = if e == e' then Just [] else Nothing
matchWith (Hole k) e = Just [(k,e)]
matchWith _ _ = Nothing

fromMExpr :: MExpr -> Expr
fromMExpr (MApp e1 e2)  = App (fromMExpr e1) (fromMExpr e2)
fromMExpr (Hole _)      = Var Pref "Hole" -- error "Hole in MExpr"
fromMExpr (Quote e)     = e

instance RewriteC a => RewriteC (MExpr -> a) where
  getRewrite rule = Rewrite {
    holes = holes . getRewrite . rule . Hole $ pid,
    rid   = pid + 1
  } where 
    pid = rid $ getRewrite (bt :: a)

-- Yet another pointless transformation
transformM :: Int -> MExpr -> MExpr
transformM _ (Quote e) = constE `a` Quote e
transformM n (Hole n') = if n == n' then idE else constE `a` Hole n'
transformM n (Quote (Var _ ".") `MApp` e1 `MApp` e2)
  | e1 `hasHole` n && not (e2 `hasHole` n) 
  = flipE `a` compE `a` e2 `c` transformM n e1
transformM n e@(MApp e1 e2) 
  | fr1 && fr2 = sE `a` transformM n e1 `a` transformM n e2
  | fr1        = flipE `a` transformM n e1 `a` e2
  | fr2, Hole n' <- e2, n' == n = e1
  | fr2        = e1 `c` transformM n e2
  | otherwise  = constE `a` e
  where
    fr1 = e1 `hasHole` n
    fr2 = e2 `hasHole` n

hasHole :: MExpr -> Int -> Bool
hasHole (MApp e1 e2) n = e1 `hasHole` n || e2 `hasHole` n
hasHole (Quote _)   _ = False
hasHole (Hole n')   n = n == n'

--
-- haddock doesn't like n+k patterns, so rewrite them
--
getVariants, getVariants' :: Rewrite -> [Rewrite]
getVariants' r@(Rewrite _ 0)  = [r]
getVariants' r@(Rewrite e nk)
    | nk >= 1    = r : getVariants (Rewrite e' (nk-1))
    | otherwise  = error "getVariants' : nk went negative"
    where
        e' = decHoles $ transformM 0 e

        decHoles (Hole n')    = Hole (n'-1)
        decHoles (MApp e1 e2) = decHoles e1 `MApp` decHoles e2
        decHoles me           = me

getVariants = getVariants' -- r = trace (show vs) vs where vs = getVariants' r

rr, rr0, rr1, rr2 :: RewriteC a => a -> a -> RewriteRule
-- use this rewrite rule and rewrite rules derived from it by iterated
-- pointless transformation
rrList :: RewriteC a => a -> a -> [RewriteRule]
rrList r1 r2 = zipWith RR (getVariants r1') (getVariants r2') where
  r1' = getRewrite r1
  r2' = getRewrite r2

rr  r1 r2 = Or          $ rrList r1 r2
rr1 r1 r2 = Or . take 2 $ rrList r1 r2
rr2 r1 r2 = Or . take 3 $ rrList r1 r2

-- use only this rewrite rule
rr0 r1 r2 = RR r1' r2' where
  r1' = getRewrite r1
  r2' = getRewrite r2
  
down, up :: RewriteRule -> RewriteRule
down = fix . Down
up   = fix . Up


idE, flipE, bindE, extE, returnE, consE, appendE, nilE, foldrE, foldlE, fstE,
  sndE, dollarE, constE, uncurryE, curryE, compE, headE, tailE, sE, commaE, 
  fixE, foldl1E, notE, equalsE, nequalsE, plusE, multE, zeroE, oneE, lengthE, 
  sumE, productE, concatE, concatMapE, joinE, mapE, fmapE, fmapIE, subtractE, 
  minusE, liftME, apE, liftM2E, seqME, zipE, zipWithE, 
  crossE, firstE, secondE, andE, orE, allE, anyE :: MExpr
idE        = Quote $ Var Pref "id"
flipE      = Quote $ Var Pref "flip"
constE     = Quote $ Var Pref "const"
compE      = Quote $ Var Inf "."
sE         = Quote $ Var Pref "ap"
fixE       = Quote $ Var Pref "fix"
bindE      = Quote $ Var Inf  ">>="
extE       = Quote $ Var Inf  "=<<"
returnE    = Quote $ Var Pref "return"
consE      = Quote $ Var Inf  ":"
nilE       = Quote $ Var Pref "[]"
appendE    = Quote $ Var Inf  "++"
foldrE     = Quote $ Var Pref "foldr"
foldlE     = Quote $ Var Pref "foldl"
fstE       = Quote $ Var Pref "fst"
sndE       = Quote $ Var Pref "snd"
dollarE    = Quote $ Var Inf  "$"
uncurryE   = Quote $ Var Pref "uncurry"
curryE     = Quote $ Var Pref "curry"
headE      = Quote $ Var Pref "head"
tailE      = Quote $ Var Pref "tail"
commaE     = Quote $ Var Inf  ","
foldl1E    = Quote $ Var Pref "foldl1"
equalsE    = Quote $ Var Inf  "=="
nequalsE   = Quote $ Var Inf  "/="
notE       = Quote $ Var Pref "not"
plusE      = Quote $ Var Inf  "+"
multE      = Quote $ Var Inf  "*"
zeroE      = Quote $ Var Pref "0"
oneE       = Quote $ Var Pref "1"
lengthE    = Quote $ Var Pref "length"
sumE       = Quote $ Var Pref "sum"
productE   = Quote $ Var Pref "product"
concatE    = Quote $ Var Pref "concat"
concatMapE = Quote $ Var Pref "concatMap"
joinE      = Quote $ Var Pref "join"
mapE       = Quote $ Var Pref "map"
fmapE      = Quote $ Var Pref "fmap"
fmapIE     = Quote $ Var Inf  "fmap"
subtractE  = Quote $ Var Pref "subtract"
minusE     = Quote $ Var Inf  "-"
liftME     = Quote $ Var Pref "liftM"
liftM2E    = Quote $ Var Pref "liftM2"
apE        = Quote $ Var Inf  "ap"
seqME      = Quote $ Var Inf  ">>"
zipE       = Quote $ Var Pref "zip"
zipWithE   = Quote $ Var Pref "zipWith"
crossE     = Quote $ Var Inf  "***"
firstE     = Quote $ Var Pref "first"
secondE    = Quote $ Var Pref "second"
andE       = Quote $ Var Pref "and"
orE        = Quote $ Var Pref "or"
allE       = Quote $ Var Pref "all"
anyE       = Quote $ Var Pref "any"



a, c :: MExpr -> MExpr -> MExpr
a       = MApp
c e1 e2 = compE `a` e1 `a` e2
infixl 9 `a`
infixr 8 `c`


collapseLists :: Expr -> Maybe Expr
collapseLists (Var _ "++" `App` e1 `App` e2)
  | (xs,x) <- getList e1, x==nil,
    (ys,y) <- getList e2, y==nil = Just $ makeList $ xs ++ ys
collapseLists _ = Nothing

data Binary = forall a b c. (Read a, Show a, Read b, Show b, Read c, Show c) => BA (a -> b -> c)

evalBinary :: [(String, Binary)] -> Expr -> Maybe Expr
evalBinary fs (Var _ f' `App` Var _ x' `App` Var _ y')
  | Just (BA f) <- lookup f' fs = (Var Pref . show) `fmap` liftM2 f (readM x') (readM y')
evalBinary _ _ = Nothing

data Unary = forall a b. (Read a, Show a, Read b, Show b) => UA (a -> b)

evalUnary :: [(String, Unary)] -> Expr -> Maybe Expr
evalUnary fs (Var _ f' `App` Var _ x')
  | Just (UA f) <- lookup f' fs = (Var Pref . show . f) `fmap` readM x'
evalUnary _ _ = Nothing

assocR, assocL, assoc :: [String] -> Expr -> Maybe Expr
-- (f `op` g) `op` h --> f `op` (g `op` h)
assocR ops (Var f1 op1 `App` (Var f2 op2 `App` e1 `App` e2) `App` e3)
  | op1 == op2 && op1 `elem` ops 
  = Just (Var f1 op1 `App` e1 `App` (Var f2 op2 `App` e2 `App` e3))
assocR _ _ = Nothing

-- f `op` (g `op` h) --> (f `op` g) `op` h
assocL ops (Var f1 op1 `App` e1 `App` (Var f2 op2 `App` e2 `App` e3))
  | op1 == op2 && op1 `elem` ops 
  = Just (Var f1 op1 `App` (Var f2 op2 `App` e1 `App` e2) `App` e3)
assocL _ _ = Nothing

-- op f . op g --> op (f `op` g)
assoc ops (Var _ "." `App` (Var f1 op1 `App` e1) `App` (Var f2 op2 `App` e2))
  | op1 == op2 && op1 `elem` ops
  = Just (Var f1 op1 `App` (Var f2 op2 `App` e1 `App` e2))
assoc _ _ = Nothing

commutative :: [String] -> Expr -> Maybe Expr
commutative ops (Var f op `App` e1 `App` e2) 
  | op `elem` ops = Just (Var f op `App` e2 `App` e1)
commutative ops (Var _ "flip" `App` e@(Var _ op)) | op `elem` ops = Just e
commutative _ _ = Nothing

-- TODO: Move rules into a file.
{-# INLINE simplifies #-}
simplifies :: RewriteRule
simplifies = Or [
  -- (f . g) x --> f (g x)
  rr0 (\f g x -> (f `c` g) `a` x)
      (\f g x -> f `a` (g `a` x)),
  -- id x --> x
  rr0 (\x -> idE `a` x)
      (\x -> x),
  -- flip (flip x) --> x
  rr  (\x -> flipE `a` (flipE `a` x))
      (\x -> x),
  -- flip id x . f --> flip f x
  rr0 (\f x -> (flipE `a` idE `a` x) `c` f)
      (\f x -> flipE `a` f `a` x),
  -- id . f --> f
  rr0 (\f -> idE `c` f)
      (\f -> f),
  -- f . id --> f
  rr0 (\f -> f `c` idE)
      (\f -> f),
  -- const x y --> x
  rr0 (\x y -> constE `a` x `a` y)
      (\x _ -> x),
  -- not (not x) --> x
  rr  (\x -> notE `a` (notE `a` x))
      (\x -> x),
  -- fst (x,y) --> x
  rr  (\x y -> fstE `a` (commaE `a` x `a` y))
      (\x _ -> x),
  -- snd (x,y) --> y
  rr  (\x y -> sndE `a` (commaE `a` x `a` y))
      (\_ y -> y),
  -- head (x:xs) --> x
  rr  (\x xs -> headE `a` (consE `a` x `a` xs))
      (\x _  -> x),
  -- tail (x:xs) --> xs
  rr  (\x xs -> tailE `a` (consE `a` x `a` xs))
      (\_ xs -> xs),
  -- uncurry f (x,y) --> f x y
  rr1 (\f x y -> uncurryE `a` f `a` (commaE `a` x `a` y))
      (\f x y -> f `a` x `a` y),
  -- uncurry (,) --> id
  rr  (uncurryE `a` commaE)
      (idE),
  -- uncurry f . s (,) g --> s f g
  rr1 (\f g -> (uncurryE `a` f) `c` (sE `a` commaE `a` g))
      (\f g -> sE `a` f `a` g),
  -- curry fst --> const
  rr (curryE `a` fstE) (constE),
  -- curry snd --> const id
  rr (curryE `a` sndE) (constE `a` idE),
  -- s f g x --> f x (g x)
  rr0 (\f g x -> sE `a` f `a` g `a` x)
      (\f g x -> f `a` x `a` (g `a` x)),
  -- flip f x y --> f y x
  rr0 (\f x y -> flipE `a` f `a` x `a` y)
      (\f x y -> f `a` y `a` x),
  -- flip (=<<) --> (>>=)
  rr0 (flipE `a` extE)
      bindE,

  -- TODO: Think about map/fmap
  -- fmap id --> id
  rr (fmapE `a` idE)
     (idE),
  -- map id --> id
  rr (mapE `a` idE)
     (idE),
  -- (f . g) . h --> f . (g . h)
  rr0 (\f g h -> (f `c` g) `c` h)
      (\f g h -> f `c` (g `c` h)),
  -- fmap f . fmap g -> fmap (f . g)
  rr0 (\f g -> fmapE `a` f `c` fmapE `a` g)
      (\f g -> fmapE `a` (f `c` g)),
  -- map f . map g -> map (f . g)
  rr0 (\f g -> mapE `a` f `c` mapE `a` g)
      (\f g -> mapE `a` (f `c` g))
  
  ]

onceRewrites :: RewriteRule
onceRewrites = Hard $ Or [
  -- ($) --> id
  rr0 (dollarE)
      idE,
  -- concatMap --> (=<<)
  rr concatMapE extE,
  -- concat    --> join
  rr concatE joinE,
  -- liftM --> fmap
  rr liftME fmapE,
  -- map --> fmap
  rr mapE fmapE,
  -- subtract -> flip (-)
  rr  subtractE
      (flipE `a` minusE)
  ]

-- Now we can state rewrite rules in a nice high level way
-- Rewrite rules should be as pointful as possible since the pointless variants
-- will be derived automatically.
rules :: RewriteRule
rules = Or [
  -- f (g x) --> (f . g) x
  Hard $
  rr  (\f g x -> f `a` (g `a` x)) 
      (\f g x -> (f `c` g) `a` x),
  -- (>>=) --> flip (=<<)
  Hard $
  rr  bindE
      (flipE `a` extE),
  -- (.) id --> id
  rr (compE `a` idE)
     idE,
  -- (++) [x] --> (:) x
  rr  (\x -> appendE `a` (consE `a` x `a` nilE))
      (\x -> consE `a` x),
  -- (=<<) return --> id
  rr  (extE `a` returnE)
      idE,
  -- (=<<) f (return x) -> f x
  rr  (\f x -> extE `a` f `a` (returnE `a` x))
      (\f x -> f `a` x),
  -- (=<<) ((=<<) f . g) --> (=<<) f . (=<<) g
  rr  (\f g -> extE `a` ((extE `a` f) `c` g))
      (\f g -> (extE `a` f) `c` (extE `a` g)),
  -- flip (f . g) --> flip (.) g . flip f
  Hard $
  rr  (\f g -> flipE `a` (f `c` g))
      (\f g -> (flipE `a` compE `a` g) `c` (flipE `a` f)),
  -- flip (.) f . flip id --> flip f 
  rr  (\f -> (flipE `a` compE `a` f) `c` (flipE `a` idE))
      (\f -> flipE `a` f),
  -- flip (.) f . flip flip --> flip (flip . f)
  rr  (\f -> (flipE `a` compE `a` f) `c` (flipE `a` flipE))
      (\f -> flipE `a` (flipE `c` f)),
  -- flip (flip (flip . f) g) --> flip (flip . flip f) g
  rr1 (\f g -> flipE `a` (flipE `a` (flipE `c` f) `a` g))
      (\f g -> flipE `a` (flipE `c` flipE `a` f) `a` g),
  
  -- flip (.) id --> id
  rr (flipE `a` compE `a` idE)
     idE,
  -- (.) . flip id --> flip flip
  rr  (compE `c` (flipE `a` idE))
      (flipE `a` flipE),
  -- s const x y --> y
  rr  (\x y -> sE `a` constE `a` x `a` y)
      (\_ y -> y),
  -- s (const . f) g --> f
  rr1 (\f g -> sE `a` (constE `c` f) `a` g)
      (\f _ -> f),
  -- s (const f) --> (.) f
  rr  (\f -> sE `a` (constE `a` f))
      (\f -> compE `a` f),
  -- s (f . fst) snd --> uncurry f
  rr  (\f -> sE `a` (f `c` fstE) `a` sndE)
      (\f -> uncurryE `a` f),
  -- fst (join (,) x) --> x
  rr (\x -> fstE `a` (joinE `a` commaE `a` x))
     (\x -> x),
  -- snd (join (,) x) --> x
  rr (\x -> sndE `a` (joinE `a` commaE `a` x))
     (\x -> x),
  -- The next two are `simplifies', strictly speaking, but invoked rarely.
  -- uncurry f (x,y) --> f x y
--  rr  (\f x y -> uncurryE `a` f `a` (commaE `a` x `a` y))
--      (\f x y -> f `a` x `a` y),
  -- curry (uncurry f) --> f
  rr (\f -> curryE `a` (uncurryE `a` f))
     (\f -> f),
  -- uncurry (curry f) --> f
  rr (\f -> uncurryE `a` (curryE `a` f))
     (\f -> f),
  -- (const id . f) --> const id
  rr  (\f -> (constE `a` idE) `c` f)
      (\_ -> constE `a` idE),
  -- const x . f --> const x
  rr (\x f -> constE `a` x `c` f)
     (\x _ -> constE `a` x),
  -- fix f --> f (fix x)
  Hard $
  rr0 (\f -> fixE `a` f)
      (\f -> f `a` (fixE `a` f)),
  -- f (fix f) --> fix x
  Hard $
  rr0 (\f -> f `a` (fixE `a` f))
      (\f -> fixE `a` f),
  -- fix f --> f (f (fix x))
  Hard $ 
  rr0 (\f -> fixE `a` f)
      (\f -> f `a` (f `a` (fixE `a` f))),
  -- fix (const f) --> f
  rr (\f -> fixE `a` (constE `a` f)) 
     (\f -> f),
  -- flip const x --> id
  rr  (\x -> flipE `a` constE `a` x)
      (\_ -> idE),
  -- const . f --> flip (const f)
  Hard $ 
  rr  (\f -> constE `c` f)
      (\f -> flipE `a` (constE `a` f)),
  -- not (x == y) -> x /= y
  rr2 (\x y -> notE `a` (equalsE `a` x `a` y))
      (\x y -> nequalsE `a` x `a` y),
  -- not (x /= y) -> x == y
  rr2 (\x y -> notE `a` (nequalsE `a` x `a` y))
      (\x y -> equalsE `a` x `a` y),
  If (Or [rr plusE plusE, rr minusE minusE, rr multE multE]) $ down $ Or [
    -- 0 + x --> x
    rr  (\x -> plusE `a` zeroE `a` x)
        (\x -> x),
    -- 0 * x --> 0
    rr  (\x -> multE `a` zeroE `a` x)
        (\_ -> zeroE),
    -- 1 * x --> x
    rr  (\x -> multE `a` oneE `a` x)
        (\x -> x),
    -- x - x --> 0
    rr  (\x -> minusE `a` x `a` x)
        (\_ -> zeroE),
    -- x - y + y --> x
    rr  (\y x -> plusE `a` (minusE `a` x `a` y) `a` y)
        (\_ x -> x),
    -- x + y - y --> x
    rr  (\y x -> minusE `a` (plusE `a` x `a` y) `a` y)
        (\_ x -> x),
    -- x + (y - z) --> x + y - z
    rr  (\x y z -> plusE `a` x `a` (minusE `a` y `a` z))
        (\x y z -> minusE `a` (plusE `a` x `a` y) `a` z),
    -- x - (y + z) --> x - y - z
    rr  (\x y z -> minusE `a` x `a` (plusE `a` y `a` z))
        (\x y z -> minusE `a` (minusE `a` x `a` y) `a` z),
    -- x - (y - z) --> x + y - z
    rr  (\x y z -> minusE `a` x `a` (minusE `a` y `a` z))
        (\x y z -> minusE `a` (plusE `a` x `a` y) `a` z)
  ],

  Hard onceRewrites,
  -- join (fmap f x) --> f =<< x
  rr (\f x -> joinE `a` (fmapE `a` f `a` x))
     (\f x -> extE `a` f `a` x),
  -- (=<<) id --> join
  rr (extE `a` idE) joinE,
  -- join --> (=<<) id
  Hard $
  rr joinE (extE `a` idE),
  -- join (return x) --> x
  rr (\x -> joinE `a` (returnE `a` x))
     (\x -> x),
  -- (return . f) =<< m --> fmap f m
  rr (\f m -> extE `a` (returnE `c` f) `a` m)
     (\f m -> fmapIE `a` f `a` m),
  -- (x >>=) . (return .) . f  --> flip (fmap . f) x
  rr (\f x -> bindE `a` x `c` (compE `a` returnE) `c` f)
     (\f x -> flipE `a` (fmapIE `c` f) `a` x),
  -- (>>=) (return f) --> flip id f
  rr (\f -> bindE `a` (returnE `a` f))
     (\f -> flipE `a` idE `a` f),
  -- liftM2 f x --> ap (f `fmap` x)
  Hard $
  rr (\f x -> liftM2E `a` f `a` x)
     (\f x -> apE `a` (fmapIE `a` f `a` x)),
  -- liftM2 f (return x) --> fmap (f x)
  rr (\f x -> liftM2E `a` f `a` (returnE `a` x))
     (\f x -> fmapIE `a` (f `a` x)),
  -- f `fmap` return x --> return (f x)
  rr (\f x -> fmapE `a` f `a` (returnE `a` x))
     (\f x -> returnE `a` (f `a` x)),
  -- (=<<) . flip (fmap . f) --> flip liftM2 f
  Hard $
  rr (\f -> extE `c` flipE `a` (fmapE `c` f))
     (\f -> flipE `a` liftM2E `a` f),
  
  -- (.) -> fmap
  Hard $ 
  rr compE fmapE,

  -- map f (zip xs ys) --> zipWith (curry f) xs ys
  Hard $
  rr (\f xs ys -> mapE `a` f `a` (zipE `a` xs `a` ys))
     (\f xs ys -> zipWithE `a` (curryE `a` f) `a` xs `a` ys),
  -- zipWith (,) --> zip (,)
  rr (zipWithE `a` commaE) zipE,

  -- all f --> and . map f
  Hard $
  rr (\f -> allE `a` f)
     (\f -> andE `c` mapE `a` f),
  -- and . map f --> all f
  rr (\f -> andE `c` mapE `a` f)
     (\f -> allE `a` f),
  -- any f --> or . map f
  Hard $
  rr (\f -> anyE `a` f)
     (\f -> orE `c` mapE `a` f),
  -- or . map f --> any f
  rr (\f -> orE `c` mapE `a` f)
     (\f -> anyE `a` f),

  -- return f `ap` x --> fmap f x
  rr (\f x -> apE `a` (returnE `a` f) `a` x)
     (\f x -> fmapIE `a` f `a` x),
  -- ap (f `fmap` x) --> liftM2 f x
  rr (\f x -> apE `a` (fmapIE `a` f `a` x))
     (\f x -> liftM2E `a` f `a` x),
  -- f `ap` x --> (`fmap` x) =<< f
  Hard $
  rr (\f x -> apE `a` f `a` x)
     (\f x -> extE `a` (flipE `a` fmapIE `a` x) `a` f),
  -- (`fmap` x) =<< f --> f `ap` x
  rr (\f x -> extE `a` (flipE `a` fmapIE `a` x) `a` f)
     (\f x -> apE `a` f `a` x),
  -- (x >>=) . flip (fmap . f) -> liftM2 f x
  rr (\f x -> bindE `a` x `c` flipE `a` (fmapE `c` f))
     (\f x -> liftM2E `a` f `a` x),

  -- (f =<< m) x --> f (m x) x
  rr0 (\f m x -> extE `a` f `a` m `a` x)
      (\f m x -> f `a` (m `a` x) `a` x),
  -- (fmap f g x) --> f (g x)
  rr0 (\f g x -> fmapE `a` f `a` g `a` x)
      (\f g x -> f `a` (g `a` x)),
  -- return x y --> y
  rr  (\y x -> returnE `a` x `a` y)
      (\y _ -> y),
  -- liftM2 f g h x --> g x `h` h x
  rr0 (\f g h x -> liftM2E `a` f `a` g `a` h `a` x)
      (\f g h x -> f `a` (g `a` x) `a` (h `a` x)),
  -- ap f id --> join f
  rr  (\f -> apE `a` f `a` idE)
      (\f -> joinE `a` f),

  -- (=<<) const q --> flip (>>) q
  Hard $ -- ??
  rr (\q p -> extE `a` (constE `a` q) `a` p)
     (\q p -> seqME `a` p `a` q),
  -- p >> q --> const q =<< p
  Hard $
  rr (\p q -> seqME `a` p `a` q)
     (\p q -> extE `a` (constE `a` q) `a` p),

  -- experimental support for Control.Arrow stuff 
  -- (costs quite a bit of performace)
  -- uncurry ((. g) . (,) . f) --> f *** g
  rr (\f g -> uncurryE `a` ((flipE `a` compE `a` g) `c` commaE `c` f))
     (\f g -> crossE `a` f `a` g),
  -- uncurry ((,) . f) --> first f
  rr (\f -> uncurryE `a` (commaE `c` f))
     (\f -> firstE `a` f),
  -- uncurry ((. g) . (,)) --> second g
  rr (\g -> uncurryE `a` ((flipE `a` compE `a` g) `c` commaE))
     (\g -> secondE `a` g),
  -- I think we need all three of them:
  -- uncurry (const f) --> f . snd
  rr (\f -> uncurryE `a` (constE `a` f))
     (\f -> f `c` sndE),
  -- uncurry const --> fst
  rr (uncurryE `a` constE)
     (fstE),
  -- uncurry (const . f) --> f . fst
  rr (\f -> uncurryE `a` (constE `c` f))
     (\f -> f `c` fstE),

  -- TODO is this the right place?
  -- [x] --> return x
  Hard $
  rr (\x -> consE `a` x `a` nilE)
     (\x -> returnE `a` x),
  -- list destructors
  Hard $ 
  If (Or [rr consE consE, rr nilE nilE]) $ Or [
    down $ Or [
      -- length [] --> 0
      rr (lengthE `a` nilE)
         zeroE,
      -- length (x:xs) --> 1 + length xs
      rr (\x xs -> lengthE `a` (consE `a` x `a` xs))
         (\_ xs -> plusE `a` oneE `a` (lengthE `a` xs))
    ],
    -- map/fmap elimination
    down $ Or [
      -- map f (x:xs) --> f x: map f xs
      rr (\f x xs -> mapE `a` f `a` (consE `a` x `a` xs))
         (\f x xs -> consE `a` (f `a` x) `a` (mapE `a` f `a` xs)),
      -- fmap f (x:xs) --> f x: Fmap f xs
      rr (\f x xs -> fmapE `a` f `a` (consE `a` x `a` xs))
         (\f x xs -> consE `a` (f `a` x) `a` (fmapE `a` f `a` xs)),
      -- map f []     --> []
      rr (\f -> mapE `a` f `a` nilE)
         (\_ -> nilE),
      -- fmap f []     --> []
      rr (\f -> fmapE `a` f `a` nilE)
         (\_ -> nilE)
    ],
    -- foldr elimination
    down $ Or [
      -- foldr f z (x:xs) --> f x (foldr f z xs)
      rr (\f x xs z -> (foldrE `a` f `a` z) `a` (consE `a` x `a` xs))
         (\f x xs z -> (f `a` x) `a` (foldrE `a` f `a` z `a` xs)),
      -- foldr f z [] --> z
      rr (\f z -> foldrE `a` f `a` z `a` nilE)
         (\_ z -> z)
    ],
    -- foldl elimination
    down $ Opt (CRR $ assocL ["."]) `Then` Or [
      -- sum xs --> foldl (+) 0 xs
      rr (\xs -> sumE `a` xs)
         (\xs -> foldlE `a` plusE `a` zeroE `a` xs),
      -- product xs --> foldl (*) 1 xs
      rr (\xs -> productE `a` xs)
         (\xs -> foldlE `a` multE `a` oneE `a` xs),
      -- foldl1 f (x:xs) --> foldl f x xs
      rr (\f x xs -> foldl1E `a` f `a` (consE `a` x `a` xs))
         (\f x xs -> foldlE `a` f `a` x `a` xs),
      -- foldl f z (x:xs) --> foldl f (f z x) xs
      rr (\f z x xs -> (foldlE `a` f `a` z) `a` (consE `a` x `a` xs))
         (\f z x xs -> foldlE `a` f `a` (f `a` z `a` x) `a` xs),
      -- foldl f z [] --> z
      rr (\f z -> foldlE `a` f `a` z `a` nilE)
         (\_ z -> z),
      -- special rule:
      -- foldl f z [x] --> f z x
      rr (\f z x -> foldlE `a` f `a` z `a` (returnE `a` x))
         (\f z x -> f `a` z `a` x),
      rr (\f z x -> foldlE `a` f `a` z `a` (consE `a` x `a` nilE))
         (\f z x -> f `a` z `a` x)
    ] `OrElse` (
      -- (:) x --> (++) [x]
      Opt (rr0 (\x -> consE `a` x)
         (\x -> appendE `a` (consE `a` x `a` nilE))) `Then`
      -- More special rule: (:) x . (++) ys --> (++) (x:ys)
      up (rr0 (\x ys -> (consE `a` x) `c` (appendE `a` ys))
         (\x ys -> appendE `a` (consE `a` x `a` ys)))
      )
  ],

  -- Complicated Transformations
  CRR (collapseLists),
  up $ Or [CRR (evalUnary unaryBuiltins), CRR (evalBinary binaryBuiltins)],
  up $ CRR (assoc assocOps),
  up $ CRR (assocL assocOps),
  up $ CRR (assocR assocOps),
  Up (CRR (commutative commutativeOps)) $ down $ Or [CRR $ assocL assocLOps,
                                                     CRR $ assocR assocROps],

  Hard $ simplifies
  ] `Then` Opt (up simplifies)
assocLOps, assocROps, assocOps :: [String]
assocLOps = ["+", "*", "&&", "||", "max", "min"]
assocROps = [".", "++"]
assocOps  = assocLOps ++ assocROps

commutativeOps :: [String]
commutativeOps = ["*", "+", "==", "/=", "max", "min"]

unaryBuiltins :: [(String,Unary)]
unaryBuiltins = [
    ("not",    UA (not    :: Bool -> Bool)),
    ("negate", UA (negate :: Integer -> Integer)),
    ("signum", UA (signum :: Integer -> Integer)),
    ("abs",    UA (abs    :: Integer -> Integer))
  ]

binaryBuiltins :: [(String,Binary)]
binaryBuiltins = [
    ("+",    BA ((+)  :: Integer -> Integer -> Integer)),
    ("-",    BA ((-)  :: Integer -> Integer -> Integer)),
    ("*",    BA ((*)  :: Integer -> Integer -> Integer)),
    ("^",    BA ((^)  :: Integer -> Integer -> Integer)),
    ("<",    BA ((<)  :: Integer -> Integer -> Bool)),
    (">",    BA ((>)  :: Integer -> Integer -> Bool)),
    ("==",   BA ((==) :: Integer -> Integer -> Bool)),
    ("/=",   BA ((/=) :: Integer -> Integer -> Bool)),
    ("<=",   BA ((<=) :: Integer -> Integer -> Bool)),
    (">=",   BA ((>=) :: Integer -> Integer -> Bool)),
    ("div",  BA (div  :: Integer -> Integer -> Integer)),
    ("mod",  BA (mod  :: Integer -> Integer -> Integer)),
    ("max",  BA (max  :: Integer -> Integer -> Integer)),
    ("min",  BA (min  :: Integer -> Integer -> Integer)),
    ("&&",   BA ((&&) :: Bool -> Bool -> Bool)),
    ("||",   BA ((||) :: Bool -> Bool -> Bool))
  ]

