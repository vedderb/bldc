{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Syntax where

import Control.Monad (foldM)
import qualified Data.ByteString.Lazy as BS
import Data.Graph (reachable)
import Data.Int
import Data.List
import qualified Data.Map as Map
import Data.Word
import Debug.Trace
import GHC.Float
import Test.QuickCheck

{- Something I foresee to be a difficulty is to generate recursive functions that are meaningful. We want them
to terminate! We can perhaps do this by generating a 'scrutinee' that we will recurse over, and attach a
base case to that. Then we need to figure out recursion strategies that always brings us closer to that
base case. -}

-- * Types

{-

int (28 bitar, 4 nollor i början, 1 av bittarna är 1 om det är en int eller uint),  float, byte,

5i32 = 32bit, 5u32 = unsigned, 5b (tror Joel) är bytes

add x y
  x' <- car x
  y' <- car y
  ....
  r = x' + y'
  r2 = car (+ x y)
  r == r2

-}

-- | Types
data Type
  = TSymbol
  | TByte
  | TInt28
  | TUInt28
  | TInt32
  | TUInt32
  | TInt64
  | TUInt64
  | TFloat
  | TDouble
  | TArray
  | TPair Type Type
  | TBool
  | TFun [Type] Type
  deriving (Show, Eq, Ord)

-- Assuming deriving Ord orders the constructors from top/smallest to bot/largest.
-- Rearranged constructors so that the ordering corresponds to LBM type promotion rules.

numericTypes :: [Type]
numericTypes =
  [ TByte,
    TUInt28,
    TInt28,
    TUInt32,
    TInt32,
    TUInt64,
    TInt64,
    TFloat,
    TDouble
  ]

isNumber :: Type -> Bool
isNumber t =
  t == TByte
    || t == TInt28
    || t == TUInt28
    || t == TInt32
    || t == TUInt32
    || t == TInt64
    || t == TUInt64
    || t == TFloat
    || t == TDouble

largestType :: [Type] -> Type
largestType ts = foldl max TByte ts

isFun :: Type -> Bool
isFun (TFun _ _) = True
isFun _ = False

domain :: Type -> Type
domain (TFun _ t) = t

args :: Type -> [Type]
args (TFun ts _) = ts

-- | Symbol
data Symbol = Sym String deriving (Show, Eq, Ord)

-- | S-Expression
data SExp f
  = -- Literals
    ILit f Integer
  | FLit f Double
  | Symbol f (Maybe f) Symbol
  | Cons f (SExp f) (SExp f)
  | Array f (BS.ByteString)
  | Quote f (SExp f) -- NOTE only quote symbols for now, and only once. Quote is also used to create lists, but we get to that later
  deriving (Show)

zeroOfType :: Type -> [SExp Type]
zeroOfType TByte = [ILit TByte 0]
zeroOfType TInt28 = [ILit TInt28 0]
zeroOfType TUInt28 = [ILit TUInt28 0]
zeroOfType TInt32 = [ILit TInt32 0]
zeroOfType TUInt32 = [ILit TUInt32 0]
zeroOfType TInt64 = [ILit TInt64 0]
zeroOfType TUInt64 = [ILit TUInt64 0]
zeroOfType TFloat = [FLit TFloat 0.0]
zeroOfType TDouble = [FLit TDouble 0.0]
zeroOfType TBool = zeroOfType TInt28 <> zeroOfType TUInt28
zeroOfType t = error $ "can not place a literal in place of :" <> show t

oneOfType :: Type -> [SExp Type]
oneOfType TByte = [ILit TByte 1]
oneOfType TInt28 = [ILit TInt28 1]
oneOfType TUInt28 = [ILit TUInt28 1]
oneOfType TInt32 = [ILit TInt32 1]
oneOfType TUInt32 = [ILit TUInt32 1]
oneOfType TInt64 = [ILit TInt64 1]
oneOfType TUInt64 = [ILit TUInt64 1]
oneOfType TFloat = [FLit TFloat 1.0]
oneOfType TDouble = [FLit TDouble 1.0]
oneOfType TBool = oneOfType TInt28 <> oneOfType TUInt28
oneOfType t = error $ "can not place a literal in place of :" <> show t

{-@
-- patterns for ctrl-c'ing into functions that need to
-- patternmatch on richer things

Define s e
Lambda params body
Closure params body env
Set s e
Setq s e
Let binds e
Progn exps
Add ops
Sub ops
Mul ops
SEq ops
SNEq ops
SAEq ops
SANEq ops
SLE ops
SLT ops
SGE ops
SGT ops
And ops
Or ops
Not op
If c thn els

@-}

-- forALls
-- shrink symbols

pattern Define :: Symbol -> SExp Type -> SExp Type
pattern Define s e <- Cons _ (Symbol _ _ (Sym "define")) (Cons _ (Symbol _ _ s) (Cons _ e (Symbol TSymbol _ (Sym "nil"))))
  where
    Define s e =
      let inner = Cons (TPair (getf e) TSymbol) e (Symbol TSymbol Nothing (Sym "nil"))
          conss = Cons (TPair TSymbol (getf inner)) (Symbol TSymbol Nothing s) inner -- maybe not nothing here... idk
       in Cons (TPair TSymbol (getf conss)) (Symbol TSymbol Nothing (Sym "define")) conss

pattern Lambda :: SExp Type -> SExp Type -> SExp Type
pattern Lambda params body <- Cons _ (Symbol _ _ (Sym "lambda")) (Cons _ params (Cons _ body (Symbol TSymbol _ (Sym "nil"))))
  where
    Lambda params body =
      let inner = Cons (TPair (getf body) TSymbol) body nil
          paramss = Cons (TPair (getf params) (getf inner)) params inner
       in Cons (TPair TSymbol (getf paramss)) (Symbol TSymbol Nothing (Sym "lambda")) paramss

pattern Closure :: SExp Type -> SExp Type -> SExp Type -> SExp Type
pattern Closure params body env <- Cons _ (Symbol _ _ (Sym "closure")) (Cons _ params (Cons _ body env))
  where
    Closure params body env =
      let env' = case env of
            Symbol _ _ (Sym "nil") -> Cons (TPair TSymbol TSymbol) env env
            _ -> env
          inner = Cons (TPair (getf body) (getf env')) body env'
          paramss = Cons (TPair (getf params) (getf inner)) params inner
       in Cons (TPair TSymbol (getf paramss)) (Symbol TSymbol Nothing (Sym "closure")) paramss

pattern Eval :: SExp Type -> SExp Type
pattern Eval e <- Cons _ (Symbol _ _ (Sym "eval")) (Cons _ e (Symbol _ _ (Sym "nil")))
  where
    Eval e = let inner = Cons (TPair (getf e) TSymbol) e (Symbol TSymbol Nothing (Sym "nil"))
             in Cons (TPair TSymbol (getf inner)) (Symbol TSymbol Nothing (Sym "eval")) inner

pattern Set :: SExp Type -> SExp Type -> SExp Type
pattern Set s e <- Cons _ (Symbol TSymbol _ (Sym "set")) (Cons _ s (Cons _ e (Symbol TSymbol _ (Sym "nil"))))
  where
    Set s e =
      let inner = Cons (TPair (getf e) TSymbol) e (Symbol TSymbol Nothing (Sym "nil"))
          ss = Cons (TPair (getf s) (getf inner)) s inner
       in Cons (TPair TSymbol (getf ss)) (Symbol TSymbol Nothing (Sym "set")) ss

pattern Setq :: SExp Type -> SExp Type -> SExp Type
pattern Setq s e <- Cons _ (Symbol TSymbol _ (Sym "setq")) (Cons _ s (Cons _ e (Symbol TSymbol _ (Sym "nil"))))
  where
    Setq s e =
      let inner = Cons (TPair (getf e) TSymbol) e (Symbol TSymbol Nothing (Sym "nil"))
          ss = Cons (TPair (getf s) (getf inner)) s inner
       in Cons (TPair TSymbol (getf ss)) (Symbol TSymbol Nothing (Sym "setq")) ss

pattern Let :: SExp Type -> SExp Type -> SExp Type
pattern Let bindings body <- Cons _ (Symbol _ _ (Sym "let")) (Cons _ bindings (Cons _ body (Symbol _ _ (Sym "nil"))))
  where
    Let bindings body =
      let inner = Cons (TPair (getf body) TSymbol) body (Symbol TSymbol Nothing (Sym "nil"))
          binds = Cons (TPair (getf bindings) (getf inner)) bindings inner
       in Cons (TPair TSymbol (getf binds)) (Symbol TSymbol Nothing (Sym "let")) binds

pattern Progn :: SExp Type -> SExp Type
pattern Progn exps <- Cons _ (Symbol TSymbol _ (Sym "progn")) exps
  where
    Progn exps = Cons (TPair TSymbol (getf exps)) (Symbol TSymbol Nothing (Sym "progn")) exps

pattern Add :: SExp Type -> SExp Type
pattern Add ops <- Cons _ (Symbol TSymbol _ (Sym "+")) ops
  where
    Add ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "+")) ops

pattern Sub :: SExp Type -> SExp Type
pattern Sub ops <- Cons _ (Symbol TSymbol _ (Sym "-")) ops
  where
    Sub ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "-")) ops

pattern Mul :: SExp Type -> SExp Type
pattern Mul ops <- Cons _ (Symbol TSymbol _ (Sym "*")) ops
  where
    Mul ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "*")) ops

pattern SEq :: SExp Type -> SExp Type
pattern SEq ops <- Cons _ (Symbol TSymbol _ (Sym "=")) ops
  where
    SEq ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "=")) ops

pattern SNEq :: SExp Type -> SExp Type
pattern SNEq ops <- Cons _ (Symbol TSymbol _ (Sym "!=")) ops
  where
    SNEq ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "!=")) ops

pattern SAEq :: SExp Type -> SExp Type
pattern SAEq ops <- Cons _ (Symbol TSymbol _ (Sym "eq")) ops
  where
    SAEq ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "eq")) ops

pattern SANEq :: SExp Type -> SExp Type
pattern SANEq ops <- Cons _ (Symbol TSymbol _ (Sym "not-eq")) ops
  where
    SANEq ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "not-eq")) ops

pattern SLE :: SExp Type -> SExp Type
pattern SLE ops <- Cons _ (Symbol TSymbol _ (Sym "<")) ops
  where
    SLE ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "<")) ops

pattern SLT :: SExp Type -> SExp Type
pattern SLT ops <- Cons _ (Symbol TSymbol _ (Sym "<=")) ops
  where
    SLT ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "<=")) ops

pattern SGE :: SExp Type -> SExp Type
pattern SGE ops <- Cons _ (Symbol TSymbol _ (Sym ">")) ops
  where
    SGE ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym ">")) ops

pattern SGT :: SExp Type -> SExp Type
pattern SGT ops <- Cons _ (Symbol TSymbol _ (Sym ">=")) ops
  where
    SGT ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym ">=")) ops

pattern And :: SExp Type -> SExp Type
pattern And ops <- Cons _ (Symbol TSymbol _ (Sym "and")) ops
  where
    And ops | otherwise = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "and")) ops

pattern Or :: SExp Type -> SExp Type
pattern Or ops <- Cons _ (Symbol TSymbol _ (Sym "or")) ops
  where
    Or ops = Cons (TPair TSymbol (getf ops)) (Symbol TSymbol Nothing (Sym "or")) ops

pattern Not :: SExp Type -> SExp Type
pattern Not e <- Cons _ (Symbol TSymbol _ (Sym "not")) (Cons _ e (Symbol _ _ (Sym "nil")))
  where
    Not e = 
      let inner = Cons (TPair (getf e) TSymbol) e (Symbol TSymbol Nothing (Sym "nil"))
      in Cons (TPair TSymbol (getf inner)) (Symbol TSymbol Nothing (Sym "not")) inner

pattern If :: SExp Type -> SExp Type -> SExp Type -> SExp Type
pattern If c thn els <- Cons _ (Symbol TSymbol _ (Sym "if")) (Cons _ c (Cons _ thn (Cons _ els (Symbol TSymbol _ (Sym "nil")))))
  where
    If c thn els =
      let inner = Cons (TPair (getf els) TSymbol) els (Symbol TSymbol Nothing (Sym "nil"))
          cthn = Cons (TPair (getf thn) (getf inner)) thn inner
          cc = Cons (TPair (getf c) (getf cthn)) c cthn
       in Cons (TPair TSymbol (getf cc)) (Symbol TSymbol Nothing (Sym "if")) cc

-- | All freeHelper symbols in an expression
-- symbols are bound by a define, lambda, or let
freeHelper :: SExp Type -> [Symbol]
freeHelper (ILit _ _) = []
freeHelper (FLit _ _) = []
freeHelper (Array _ _) = []
freeHelper (Symbol _ _ s) = [s]
freeHelper (Quote _ s) = freeHelper s
freeHelper (Define s e) = freeHelper e \\ [s]
freeHelper (Lambda pars e) = freeHelper e \\ map (\(Symbol _ _ s) -> s) (init $ consToList pars)
freeHelper (Let binds e) = (freeHelper e `union` freeHelper binds) \\ map (\(Cons _ (Symbol _ _ s) _) -> s) (init $ consToList binds)
freeHelper (Closure params body env) = (freeHelper body `union` freeHelper env) \\ map (\(Symbol _ _ s) -> s) (init $ consToList params) 
freeHelper (Set s e) = freeHelper s `union` freeHelper e
freeHelper (Setq s e) = freeHelper s `union` freeHelper e
freeHelper (Progn exps) = foldl' union [] (map freeHelper (init $ consToList exps))
freeHelper (Add ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (Sub ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (Mul ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (SEq ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (SNEq ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (SAEq ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (SANEq ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (SLE ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (SLT ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (SGE ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (SGT ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (And ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (Or ops) = foldl' union [] (map freeHelper (init $ consToList ops))
freeHelper (Not op) = freeHelper op
freeHelper (If c thn els) = freeHelper c `union` freeHelper thn `union` freeHelper els
freeHelper (Cons _ e1 e2) = freeHelper e1 `union` freeHelper e2

free :: SExp Type -> [Symbol]
free e = freeHelper e \\ keywords

keywords :: [Symbol]
keywords =
  map
    Sym
    [ "nil",
      "define",
      "lambda",
      "let",
      "closure",
      "set",
      "setq",
      "let",
      "progn",
      "+",
      "-",
      "*",
      "mod",
      "/",
      "eq",
      "not-eq",
      "=",
      "!=",
      "<",
      "<=",
      ">",
      ">=",
      "and",
      "or",
      "not",
      "if",
      "quote",
      "eval"
    ]


-- | Assuming that the S-expression is a top-level definition, return the symbols it
-- defines. This will be used e.g. for shrinking. If an expression depends on a symbol x,
-- this function can be used to identify another expression as one that must be kept.
-- NOTE: can only create top-level defines by define atm (always?)
toplevelDefines :: SExp Type -> [Symbol]
toplevelDefines (Define s _) = [s]
toplevelDefines _ = []

-- | Shrink an S-expression into a list of smaller S-expressions, that are still
-- well-formed and of the same type
shrinkSExp :: SExp Type -> [SExp Type]
shrinkSExp (ILit t i) = [ILit t i' | i' <- shrink i]
shrinkSExp (FLit t f) = [FLit t f' | f' <- shrink f]
shrinkSExp (Array t a) = [] -- we do not generate arrays yet
shrinkSExp (Quote t s) = [] -- can not shrink Quote
shrinkSExp (Define s e) = [Define s e' | e' <- shrinkSExp e]
shrinkSExp (Lambda pars e) = [Lambda pars e' | e' <- shrinkSExp e]
shrinkSExp (Eval e) = []
shrinkSExp (Let binds e) =
  shrinkAwayTheLet
    <> shrinkNumLocalVars
    <> [ Let binds e'
         | e' <- shrinkSExp e
       ]
    <> [ Let binds' e
         | binds' <- shrinkSExp binds
       ]
  where
    -- \| if none of the let-bound variables are used in the body, remove the entire let
    shrinkAwayTheLet :: [SExp Type]
    shrinkAwayTheLet =
      let bound = map (\(Cons _ (Symbol _ _ s) _) -> s) (init $ consToList binds)
          used = free e
       in if all (\s -> not $ s `elem` bound) used
            then [e]
            else []

    -- \| try to shrink the number of bound local variables. Only remove those that are never used
    shrinkNumLocalVars :: [SExp Type]
    shrinkNumLocalVars =
      [ Let (listToCons binds') e
        | binds' <-
            let l = init (consToList binds)
             in if not (null l)
                  then go [] (head l) (tail l)
                  else [],
          not (null binds')
      ]
      where
        -- \| traverse each of the local-var bindings and see if it does not occur in e. If so,
        -- return a sublist of the input list, that does not contain that local variable
        go :: [SExp Type] -> SExp Type -> [SExp Type] -> [[SExp Type]]
        go pref c@(Cons _ (Symbol _ _ s) _) [] =
          if s `elem` free e
            then []
            else [pref]
        go pref c@(Cons _ (Symbol _ _ s) _) suff =
          if s `elem` free e
            then go (pref <> [c]) (head suff) (tail suff)
            else pref <> suff : go (pref <> [c]) (head suff) (tail suff)
shrinkSExp (Progn exps) =
  let expsl = init $ consToList exps
      start = init expsl
      l = last expsl
   in l
        : [ Progn (listToCons (start' <> [l]))
            | start' <- shrinkList shrinkSExp start, not (null start')
          ]
          <> [Progn (listToCons [l']) | l' <- shrinkSExp l]
shrinkSExp (Add ops) =
  (init $ consToList ops)
    <> [ Add (listToCons ops')
         | ops' <-
             let asList = init $ consToList ops
              in shrinkList shrinkSExp asList,
           not (null ops')
       ]
shrinkSExp (Sub ops) =
  (init $ consToList ops)
    <> [ Sub (listToCons ops')
         | ops' <-
             let asList = init $ consToList ops
              in shrinkList shrinkSExp asList,
           not (null ops')
       ]
shrinkSExp (Mul ops) =
  (init $ consToList ops)
    <> [ Mul (listToCons ops')
         | ops' <-
             let asList = init $ consToList ops
              in shrinkList shrinkSExp asList,
           not (null ops')
       ]
shrinkSExp (SEq ops) =
  [ SEq (listToCons ops')
    | ops' <-
        let asList = init $ consToList ops
         in shrinkList shrinkSExp asList,
      not (null ops')
  ]
shrinkSExp (SNEq ops) =
  [ SNEq (listToCons ops')
    | ops' <-
        let asList = init $ consToList ops
         in shrinkList shrinkSExp asList,
      not (null ops')
  ]
shrinkSExp (SAEq ops) =
  [ SAEq (listToCons ops')
    | ops' <-
        let asList = init $ consToList ops
         in shrinkList shrinkSExp asList,
      not (null ops')
  ]
shrinkSExp (SANEq ops) =
  [ SANEq (listToCons ops')
    | ops' <-
        let asList = init $ consToList ops
         in shrinkList shrinkSExp asList,
      not (null ops')
  ]
shrinkSExp (SLE ops) =
  [ SLE (listToCons ops')
    | ops' <-
        let asList = init $ consToList ops
         in shrinkList shrinkSExp asList,
      not (null ops')
  ]
shrinkSExp (SLT ops) =
  [ SLT (listToCons ops')
    | ops' <-
        let asList = init $ consToList ops
         in shrinkList shrinkSExp asList,
      not (null ops')
  ]
shrinkSExp (SGE ops) =
  [ SGE (listToCons ops')
    | ops' <-
        let asList = init $ consToList ops
         in shrinkList shrinkSExp asList,
      not (null ops')
  ]
shrinkSExp (SGT ops) =
  [ SGT (listToCons ops')
    | ops' <-
        let asList = init $ consToList ops
         in shrinkList shrinkSExp asList,
      not (null ops')
  ]
shrinkSExp (And ops) =
  (init $ consToList ops)
    <> [ And (listToCons ops')
         | ops' <-
             let asList = init $ consToList ops
              in shrinkList shrinkSExp asList,
           not (null ops')
       ]
shrinkSExp (Or ops) =
  (init $ consToList ops)
    <> [ Or (listToCons ops')
         | ops' <-
             let asList = init $ consToList ops
              in shrinkList shrinkSExp asList,
           not (null ops')
       ]
shrinkSExp (Not e) = e : [Not e' | e' <- shrinkSExp e]
shrinkSExp (If c thn els) =
  thn
    : els
    : [If c thn' els | thn' <- shrinkSExp thn]
      <> [If c thn els' | els' <- shrinkSExp els]
      <> [If c' thn els | c' <- shrinkSExp c]
shrinkSExp (Set s e) = e : [Set s e' | e' <- shrinkSExp e]
shrinkSExp (Setq s e) = e : [Setq s e' | e' <- shrinkSExp e]
shrinkSExp (Cons t e1 e2) =
  [Cons (TPair (getf e1') (getf e2)) e1' e2 | e1' <- shrinkSExp e1] <>
  [Cons (TPair (getf e1) (getf e2')) e1 e2' | e2' <- shrinkSExp e2]
shrinkSExp (Symbol _ (Just t) _) | isNumber t || t == TBool = zeroOfType t <> oneOfType t -- if possible, replace symbols with literals
shrinkSExp (Symbol _ _ _) = []

shrinkCtx :: [Symbol] -> Ctx -> [Ctx]
shrinkCtx dependent ctx = strategy1 dependent ctx <> strategy2 ctx

-- | Remove entire S-expressions from the environment
strategy1 :: [Symbol] -> Ctx -> [Ctx]
strategy1 dependent ctx
  | length (toplevel ctx) >= 1 =
      [ ctx {toplevel = l}
        | l <- go dependent [] (head (toplevel ctx)) (tail (toplevel ctx)) --,
       --   not (null (toplevel ctx))
      ]
  where
    go :: [Symbol] -> [SExp Type] -> SExp Type -> [SExp Type] -> [[SExp Type]]
    go mustkeep pref c [] =
      if not $ null $ mustkeep `intersect` toplevelDefines c
        then []
        else [pref]
    go mustkeep pref c suff =
      if not $ null $ mustkeep `intersect` toplevelDefines c -- if this S-expression defines a symbol we depend on
        then go (nub $ free c <> mustkeep) (pref <> [c]) (head suff) (tail suff) -- we keep it and recurse
        else (pref <> suff) : go (nub $ free c <> mustkeep) (pref <> [c]) (head suff) (tail suff) -- otherwise we produce
        -- a new list without it, and recurse
strategy1 _ _ = []

-- | Strategy 2 tries to shrink each S-expression at the toplevel, in turn
strategy2 :: Ctx -> [Ctx]
strategy2 ctx = [ctx {toplevel = l} | l <- applyElementWise shrinkSExp (toplevel ctx)]

applyElementWise :: (a -> [a]) -> [a] -> [[a]]
applyElementWise _ [] = []
applyElementWise f xs = go [] (head xs) (tail xs)
  where
    go pref c [] = [pref ++ [c'] | c' <- f c]
    go pref c suff = [pref <> [c'] <> suff | c' <- f c] <> go (pref <> [c]) (head suff) (tail suff)

--      (pref <> shrinkSExp c <> suff) : go (pref <> [c]) (head suff) (tail suff)

-- Quotes are a bit special. Maybe we need to talk about those.
{-
(define a 'bepa) results in the binding a is b in the env,
(define a ''bepa) results in the binding a is '(quote b)` in the env
(define a '1) => a is 1
(define a '(+ 1 2)) => a is (+ 1 2)
(define a (+ 1 2) => a is 3

the tickmark ' is processed by the parser and can never really occur in the environment
there it will be present as (quote something) or not at all.

So I am wondering if in the syntax, maybe quote should just be a symbol.
it may be different if we want to generate code to be parsed.
I dont know :/

Need to think about the whole family ' ` , ,@  of things that are dealt with "mostly"
while reading.
-}

{- t1 == t2 && (abs (i - j)) < 0.0001
   Makes sense for small doubles. 
-}

instance (Eq t) => Eq (SExp t) where
  (==) (ILit t1 i) (ILit t2 j) = t1 == t2 && i == j
  (==) (FLit t1 i) (FLit t2 j) = t1 == t2 && neweq i j
    -- let (a', b') = if (i > j) then (i, j) else (j, i)
    --     (a, b)   = if (a' < 0 && b' < 0) then (-a' , -b') else (a', b')
    --     is_small = a < 100000.0 || b < 100000.0
    --     in if (is_small)
    --        then t1 == t2 && (abs (i - j)) < 0.0001
    --        else t1 == t2 && (1.0 - (a / b)) < 0.0001
  (==) (Symbol t1 _ s1) (Symbol t2 _ s2) = t1 == t2 && s1 == s2
  (==) (Cons t1 a1 a2) (Cons t2 b1 b2) = t1 == t2 && a1 == b1 && a2 == b2
  (==) (Array t1 a) (Array t2 b) = t1 == t2 && a == b
  (==) (Quote t1 s1) (Quote t2 s2) = t1 == t2 && s1 == s2
  (==) _ _ = False

neweq :: Double -> Double -> Bool
neweq d1 d2 | d1 == d2 = True
            | otherwise = abs (d1 - d2) < max ((min d1 d2) * 1e-16) 1e-16

-- neweq :: Double -> Double -> Bool
-- neweq d1 d2
--   | d1 == d2 = True
--   | otherwise = diff < max abs_th (epsilon * norm)
--   where
--     epsilon = 0.05
--     abs_th = 1
--     diff = abs (d1 - d2)
--     norm = min (abs d1 + abs d2) (10.0)

-- Construct values

i28 :: (Integral a) => a -> SExp Type
i28 a = ILit TInt28 (fromIntegral a)

u28 :: (Integral a) => a -> SExp Type
u28 a = ILit TUInt28 (fromIntegral a)

i32 :: (Integral a) => a -> SExp Type
i32 a = ILit TInt32 (fromIntegral a)

u32 :: (Integral a) => a -> SExp Type
u32 a = ILit TUInt32 (fromIntegral a)

i64 :: (Integral a) => a -> SExp Type
i64 a = ILit TInt64 (fromIntegral a)

u64 :: (Integral a) => a -> SExp Type
u64 a = ILit TUInt64 (fromIntegral a)

f32 :: (Real a) => a -> SExp Type
f32 a = FLit TFloat (realToFrac a)

f64 :: (Real a) => a -> SExp Type
f64 a = FLit TDouble (realToFrac a)

-- Null terminated C string.
str :: String -> SExp Type
str s =
  Array
    TArray
    ( BS.concat
        [ BS.pack (map (toEnum . fromEnum) s),
          BS.pack [0 :: Word8]
        ]
    )

--  | List f [SExp f]

listToCons :: [SExp Type] -> SExp Type
listToCons [] = nil
listToCons [e] = Cons (TPair (getf e) (getf nil)) e nil
listToCons (e : es) = let r = listToCons es in Cons (TPair (getf e) (getf r)) e r

cons :: SExp Type -> SExp Type -> SExp Type
cons a b = Cons (TPair (getf a) (getf b)) a b

consToList :: SExp Type -> [SExp Type]
consToList (Cons _ e1 e2) = e1 : consToList e2
consToList e = [e]

setListType :: Type -> SExp Type -> SExp Type
setListType t (Cons _ e1 e2) = Cons t e1 e2
setListType _ _ = error "trying to set the type of a list, but the thing is not actually a list..."

typeOfLam :: SExp Type -> Type
typeOfLam (Cons f _ _) = f

-- typeOfLam (List (TList (t : _)) _) = t

-- | Fetch the functor value out of an expression
-- NOTE we have to be careful when we use this. We have 'phantom types' in the sense that we can construct
-- types during generation that are actually not valid lisp types. We must make sure that the constructed
-- syntax does not contain these types (TFun, TBool). If you need to get the corresponding type information you
-- must construct it some other way.
getf :: SExp f -> f
getf (ILit f _) = f
getf (FLit f _) = f
getf (Symbol f _ _) = f
getf (Cons f _ _) = f
getf (Quote f _) = f

-- getf (List f _) = f

-- * Pretty-print

prettyType :: Type -> String
prettyType t = case t of
  TByte -> "type-char"
  TInt28 -> "type-i28"
  TUInt28 -> "type-i28"
  TInt32 -> "type-i32"
  TUInt32 -> "type-u32"
  TInt64 -> "type-i64"
  TUInt64 -> "type-u64"
  TFloat -> "type-float"
  TDouble -> "type-double"
  TSymbol -> "type-symbol"
  TBool -> "type-symbol"
  TPair _ _ -> "type-list"
  TFun ts t -> show (TFun ts t) --"type-list"

prettySymbol :: Symbol -> String
prettySymbol (Sym str) = str

prettyValQual :: Type -> String
prettyValQual TByte = "b"
prettyValQual TInt28 = ""
prettyValQual TUInt28 = "u"
prettyValQual TInt32 = "i32"
prettyValQual TUInt32 = "u32"
prettyValQual TInt64 = "i64"
prettyValQual TUInt64 = "u64"
prettyValQual TFloat = "f32"
prettyValQual TDouble = "f64"

prettyExp :: SExp Type -> String
prettyExp (Symbol _ _ (Sym s)) = s
prettyExp (ILit t i)
  | i < 0 = "-" <> show (abs i) <> prettyValQual t
  | otherwise = show i <> prettyValQual t
prettyExp (FLit t f)
  | f < 0.0 = "-" <> show (abs f) <> prettyValQual t
  | otherwise = show f <> prettyValQual t
prettyExp (Define (Sym s) b@(ILit _ _)) | "env-var" `isPrefixOf` s = "(define " <> s <> " " <> prettyExp b <> ")"
prettyExp (Define (Sym s) b@(FLit _ _)) | "env-var" `isPrefixOf` s = "(define " <> s <> " " <> prettyExp b <> ")"
prettyExp (Define (Sym s) b) | "env-var" `isPrefixOf` s = "(define " <> s <> " '" <> prettyExp b <> ")"
prettyExp (Define (Sym s) b) | otherwise = "(define " <> s <> " " <> prettyExp b <> ")"
prettyExp e@(Cons _ a b) = "(" <> prettyExp a <> prettyList b <> ")"
prettyExp (Quote _ e) = "'" <> prettyExp e

prettyList :: SExp Type -> String
prettyList (Symbol _ _ (Sym "nil")) = ""
prettyList (Cons _ a (Symbol _ _ (Sym "nil"))) = " " <> prettyExp a
prettyList (Cons _ a e@(Cons _ _ _)) = " " <> prettyExp a <> prettyList e
prettyList (Cons _ a b) = " " <> prettyExp a <> prettyList b
prettyList b = " . " <> prettyExp b

-- | wrap something in parenthesis that needs to be treated as a single entity
parens :: SExp Type -> String
parens e@(ILit _ i) | i < 0 = "(" <> prettyExp e <> ")"
parens e@(FLit _ f) | f < 0.0 = "(" <> prettyExp e <> ")"
parens e = prettyExp e

prettyCtx :: Ctx -> String
prettyCtx ctx = intercalate "\n\n" (map prettyExp (toplevel ctx))

data Op = Op String Integer Integer

fundamentals :: [Op]
fundamentals =
  [ Op "+" 0 10,
    Op "-" 0 10,
    Op "*" 0 10,
    Op "/" 0 10,
    Op "mod" 0 10,
    Op "=" 0 10,
    Op "!=" 0 10,
    Op "<" 0 10 -- + many more ?
  ]

-- * Constant symbols

mkSymbol :: String -> SExp Type
mkSymbol str = Symbol TSymbol Nothing (Sym str)

define :: Symbol
define = Sym "define"

lambda :: Symbol
lambda = Sym "lambda"

progn :: Symbol
progn = Sym "progn"

t :: Symbol
t = Sym "t"

true :: SExp Type
true = Symbol TSymbol Nothing t -- Maybe TBool here?

false :: SExp Type
false = nil

nil :: SExp Type
nil = Symbol TSymbol Nothing (Sym "nil") -- Nil TSymbol -- Maybe TSymbol

gc :: SExp Type
gc = listToCons [mkSymbol "gc"]

flatten :: SExp Type
flatten = Symbol TSymbol Nothing (Sym "flatten")

unflatten :: SExp Type
unflatten = Symbol TSymbol Nothing (Sym "unflatten")

lispIf :: SExp Type -> SExp Type -> SExp Type -> SExp Type
lispIf t e1 e2 = listToCons [ mkSymbol "if", t, e1, e2 ]

lispCond :: [SExp Type] -> [SExp Type] -> SExp Type
lispCond cs es = listToCons ((mkSymbol "cond") : zipWith condition cs es)
  where
    condition c e = listToCons [c , e]

lispEval :: SExp Type -> SExp Type
lispEval e = listToCons [mkSymbol "eval", e]

lispQuote :: SExp Type -> SExp Type
lispQuote e = listToCons [mkSymbol "quote", e]
                    

lispProgn :: [SExp Type] -> SExp Type
listProgn [] = nil
lispProgn exps = 
  let e1 = mkSymbol "progn"
      rest = listToCons exps
   in Cons (TPair (getf e1) (getf rest)) e1 rest

arithmeticOpInt :: [SExp Type]
arithmeticOpInt = arithmeticOpGeneric TInt28

arithmeticOpFloat :: [SExp Type]
arithmeticOpFloat = arithmeticOpGeneric TFloat

arithmeticOpGeneric :: Type -> [SExp Type]
arithmeticOpGeneric t =
  [ Symbol TSymbol Nothing (Sym "+"),
    Symbol TSymbol Nothing (Sym "-")
    --    Symbol (TFun [t, t] t) (Sym "/"), -- TODO fixme, talk to joel, how to generate wellformed divs
    -- Symbol TSymbol (Sym "mod")
  ]

booleanOps :: [SExp Type]
booleanOps = undefined

-- * Instances

instance Functor SExp where
  fmap f (ILit a i) = ILit (f a) i
  fmap f (FLit a d) = FLit (f a) d
  fmap f (Symbol a ma s) = Symbol (f a) (fmap f ma) s
  fmap f (Cons a e1 e2) = Cons (f a) (fmap f e1) (fmap f e2)
  fmap f (Quote a e) = Quote (f a) (fmap f e)

-- * Generator context

data Ctx = Ctx
  { -- | top level definitions of the generated file
    toplevel :: [SExp Type],
    -- | integer used to generate unique names
    namegen :: Int,
    -- | symbols that are in scope during generation
    inscope :: [[(Symbol, Type, Bool)]] -- the bool signifies whether we need eval or not
  }
  deriving (Show)

newCtx :: Ctx
newCtx = Ctx [] 0 [[]]

fresh :: Ctx -> String -> (Ctx, Symbol)
fresh ctx base = let n = namegen ctx in (ctx {namegen = n + 1}, Sym $ base <> show n)

freshN :: Ctx -> String -> Int -> (Ctx, [Symbol])
freshN ctx base 1 = let (c, n) = fresh ctx base in (c, [n])
freshN ctx base n
  | n > 1 =
      let (ctx', na) = fresh ctx base
          (ctx'', ns) = freshN ctx' base (n - 1)
       in (ctx'', na : ns)

-- | Run a generator in an extended environment
withLocal :: Ctx -> Symbol -> Type -> (Ctx -> Gen (Ctx, a)) -> Gen (Ctx, a)
withLocal ctx s t g = do
  let ctx' = ctx {inscope = [(s, t, False)] : (inscope ctx)}
  (ctx'', a) <- g ctx'
  return $ (ctx'' {inscope = tail (inscope ctx'')}, a) -- Map.delete s (inscope ctx'')}, a)

withLocals :: Ctx -> [Symbol] -> [Type] -> (Ctx -> Gen (Ctx, a)) -> Gen (Ctx, a)
withLocals ctx names types g = do
  let topmost = zip3 names types (repeat False)
      ctx' = ctx {inscope = topmost : inscope ctx}
  (ctx'', a) <- g ctx'
  return (ctx'' {inscope = tail (inscope ctx'')}, a)

-- | All the symbols that can be reached from the current context. They are associated with
-- a number meant to be given to `frequency`. The frequency exponentially gets higher the
-- 'closer' you are to the binding point of a symbol.
--
-- (define f (lambda (x) (+ x 1)))
--
-- in the above example, x and f are in scope. x has a frequency of 4, whereas f has
-- a frequency of 2. This is meant to promote the usage of local variables over global ones.
reachableSymbols :: Ctx -> [(Int, SExp Type, Bool)]
reachableSymbols ctx =
  concat
    [ map (\(s, t, b) -> (2 ^ i, Symbol TSymbol (Just t) s, b)) sc
      | (sc, i) <- zip (reverse $ inscope ctx) [1 ..]
    ]

toplevelToEnv :: Ctx -> Map.Map Symbol (SExp Type)
toplevelToEnv ctx =
  Map.fromList $
    concatMap
      ( \e ->
          case e of
            Define s e -> [(s, lambdaToClosure e)]
            _ -> []
      )
      (toplevel ctx)

testConv :: IO ()
testConv = do
  (s, _) <- generate $ genFunction newCtx 1 TInt28
  putStrLn $ "before: " <> prettyExp s
  putStrLn $ "after:  " <> prettyExp (lambdaToClosure s)

lambdaToClosure :: SExp Type -> SExp Type
lambdaToClosure (Lambda params body) = Closure params body (cons nil nil)
lambdaToClosure e = e

-- * Generator

instance Arbitrary Type where
  arbitrary = arbSizedType 15

arbSizedType :: Int -> Gen Type
arbSizedType 0 =
  frequency
    [ (2, genNumericType),
      (1, return TSymbol),
      (1, return TBool)
    ]
arbSizedType n =
  frequency
    [ (2, genNumericType),
      (1, return TSymbol),
      (1, return TBool) --,
      -- (2, do
      --   nt <- chooseInt (1,n)
      --   ts <- sequence $ replicate nt (arbSizedType (n `div` nt))
      --   d <- arbSizedType (n `div` nt)
      --   return $ TFun ts d)
    ]

genNumericLTE :: Type -> Gen Type
genNumericLTE t = elements $ filter (\t' -> t' <= t) numericTypes

genNumericType :: Gen Type
genNumericType = elements numericTypes

genType :: Int -> Gen Type
genType 1 = genNumericType
genType n =
  do
    i <- chooseInt(0,10)
    case i of
      0 -> do
        t1 <- genType (n-1)
        t2 <- genType (n-1)
        return $ TPair t1 t2
      _ -> genNumericType
  
requiresEval :: Ctx -> SExp Type -> Bool
requiresEval ctx e = case e of
    (ILit _ _) -> False
    (FLit _ _) -> False
    e -> True
  where
    requiresEval' :: Symbol -> Bool
    requiresEval' s = case find (\(s',_,b) -> s == s') (concat $ inscope ctx) of
      Just (_,_,b) -> b
      Nothing -> False

-- | Generate literal (poorly named, this is really generating either constant
-- literals or define's that populate the global environment)
genLiteral :: Ctx -> Gen (Ctx, SExp Type)
genLiteral ctx = do
  t <- arbitrary
  s <- chooseInt (1,5)
  (ctx', e) <- genExp ctx s t
  oneof
    [ return (ctx', e),
      let (ctx'', n) = fresh ctx' "env-var"
       in do
            let exps = [Symbol TSymbol Nothing define, Symbol TSymbol Nothing n, e]
                toeval = requiresEval ctx'' e -- do we need to call eval when we draw upon this variable?
                ctx''' = ctx'' { inscope = ((n,t,toeval) : (head (inscope ctx''))) : tail (inscope ctx'')}
            return (ctx''', listToCons exps)
--            return (extendScope ctx'' n t, listToCons exps)
    ]

-- | invoke genLiteral and add the s-exp to the toplevel definitions
genEnvConstant :: Ctx -> Gen Ctx
genEnvConstant ctx = do
  (ctx', e) <- genLiteral ctx
  return $ ctx' {toplevel = e : toplevel ctx'}

-- | Generate a function (lambda)
genFunction :: Ctx -> Int -> Type -> Gen (SExp Type, Type)
genFunction ctx n d = do
  numparams <- chooseInt (1, 4)
  pts <- sequence $ replicate numparams arbitrary

  let (ctx', ns) = foldl (\(ctx, names) pt -> let (ctx', name) = fresh ctx "p" in (ctx', name : names)) (ctx, []) pts

  (ctx'', e) <- withLocals ctx' ns pts $ \c -> genExp c n d
  let funl = [Symbol TSymbol Nothing lambda, listToCons (map (\n -> Symbol TSymbol Nothing n) ns), e]
  return (listToCons funl, TFun pts d)

genEnvFunction :: Ctx -> Gen Ctx
genEnvFunction ctx = do
  t <- arbitrary
  s <- chooseInt (1, 4)
  (lam, funt) <- genFunction ctx s t
  let (ctx', f) = fresh ctx "env-fun"
      exps = [Symbol TSymbol Nothing define, Symbol TSymbol Nothing f, lam]
      finexp = listToCons exps
  return $
    ctx'
      { toplevel = finexp : toplevel ctx',
        inscope = ((f, funt, False) : (head (inscope ctx'))) : tail (inscope ctx')
      }

genList :: Ctx -> Int -> Int -> Gen (Ctx, SExp Type)
genList ctx s 0 = return (ctx, Symbol TSymbol Nothing (Sym "nil"))
genList ctx s len = do
  t <- genType s
  (ctx', e) <- genExp ctx s t
  (ctx'', e0) <- genList ctx s (len - 1)
  return $ (ctx'', Cons (TPair (getf e) (getf e0)) e e0)
  
genExps :: Ctx -> Int -> [Type] -> Gen (Ctx, [SExp Type])
genExps ctx n [] = return (ctx, [])
genExps ctx n (t : ts) = do
  (ctx', es) <- genExps ctx n ts
  (ctx'', e) <- genExp ctx n t
  return (ctx'', e : es)

genExp :: Ctx -> Int -> Type -> Gen (Ctx, SExp Type)
genExp ctx n t
  | n <= 0 =
      let l = filter (\(_, Symbol _ (Just t') _,_) -> t' == t) $ reachableSymbols ctx
          gens = map (\(i, s,b) -> (i, return $ if b then Eval s else s)) l
       in case t of
            TByte -> numBaseGenerator gens t
            TUInt28 -> numBaseGenerator gens t
            TInt28 -> numBaseGenerator gens t
            TUInt32 -> numBaseGenerator gens t
            TInt32 -> numBaseGenerator gens t
            TUInt64 -> numBaseGenerator gens t
            TInt64 -> numBaseGenerator gens t
            TFloat -> numBaseGenerator gens t
            TDouble -> numBaseGenerator gens t
            TBool -> do
              s <- frequency ((1, return true) : (1, return false) : if not (null l) then gens else [])
              return (ctx, s)
            TPair t1 t2 -> do
              (ctx', a) <- genExp ctx (n - 1) t1
              (ctx'', b) <- genExp ctx' (n - 1) t2
              return (ctx'', cons a b)
            TSymbol -> do
              e <- elements $ [true, false]
              return (ctx, e)
            TFun ts t2 -> do --error $ concat ["can not generate expression of type " <> show t <> "of size 0"]
              let (ctx', ns) = foldl (\(ctx, names) pt -> let (ctx', name) = fresh ctx "p" in (ctx', name : names)) (ctx, []) ts
              (ctx'', e) <- withLocals ctx' ns ts $ \c -> genExp c (n - 1) t2
              let funl = [Symbol TSymbol Nothing lambda, listToCons (map (\n -> Symbol TSymbol Nothing n) ns), e]
              return (ctx'', listToCons funl)
  where
    numBaseGenerator :: [(Int, Gen (SExp Type))] -> Type -> Gen (Ctx, SExp Type)
    numBaseGenerator inscopes t = do
      lg <- case t of
        TByte -> return $ ILit TByte <$> (toInteger <$> (arbitrary :: Gen Word8))
        TUInt28 -> return $ ILit TUInt28 <$> chooseInteger (0, 268435456)
        TInt28 -> return $ ILit TInt28 <$> chooseInteger (-134217728, 134217728)
        TUInt32 -> return $ ILit TUInt32 <$> (toInteger <$> (arbitrary :: Gen Word32))
        TInt32 -> return $ ILit TInt32 <$> (toInteger <$> (arbitrary :: Gen Int32))
        TUInt64 -> return $ ILit TUInt64 <$> (toInteger <$> (arbitrary :: Gen Word64))
        TInt64 -> return $ ILit TInt64 <$> (toInteger <$> (arbitrary :: Gen Int64))
        TFloat -> return $ FLit TFloat <$> (float2Double <$> (arbitrary :: Gen Float))
        TDouble -> return $ FLit TDouble <$> arbitrary
      s <- frequency $ (1, lg) : if not (null inscopes) then inscopes else []
      return (ctx, s)
genExp ctx n t = case t of
  TByte -> numGenerator t
  TUInt28 -> numGenerator t
  TInt28 -> numGenerator t
  TUInt32 -> numGenerator t
  TInt32 -> numGenerator t
  TUInt64 -> numGenerator t
  TInt64 -> numGenerator t
  TFloat -> numGenerator t
  TDouble -> numGenerator t
  TBool ->
    frequency
      [ (1, genExp ctx (n - 1) t),
        (1, (,) ctx <$> genFunApp ctx (n - 1) t),
        ( 1,
          do
            t <- arbitrary
            (ctx', e) <- genExp ctx (n - 1) t
            let exps = [Symbol TSymbol Nothing (Sym "not"), e]
            return (ctx', listToCons exps)
        ),
        (1, genIf),
        (1, genLet),
        (1, genSetvar),
        (2, genProgn),
        ( 2,
          do
            numexps <- chooseInt (1, 6)
            ts <- sequence $ replicate numexps genNumericType
            (ctx', es) <- genExps ctx (n `div` numexps) ts
            f <- elements $ map (Symbol TSymbol Nothing . Sym) ["=", "!=", ">", ">=", "<", "<="]
            return (ctx', listToCons (f : es))
        ),
        ( 2,
          do
            numexps <- chooseInt (1, 6)
            ts <- sequence $ replicate numexps arbitrary -- generate types
            (ctx', es) <- genExps ctx (n `div` numexps) ts
            symbol <- elements $ map (Symbol TSymbol Nothing . Sym) ["and", "or", "eq", "not-eq"] -- [Symbol TSymbol (Sym "and"), Symbol TSymbol (Sym "or")]
            let exps = symbol : es
            return (ctx', listToCons exps)
        )
      ]
  TPair t1 t2 -> do
    (ctx', e1) <- genExp ctx (n - 1) t1
    (ctx'', e2) <- genExp ctx' (n - 1) t2
    return (ctx'', Cons (TPair t1 t2) e1 e2)
  TSymbol -> genExp ctx (n - 1) t
  TFun ts t2 -> do -- this and the base case are the same, they should be broken out
    let (ctx', ns) = foldl (\(ctx, names) pt -> let (ctx', name) = fresh ctx "p" in (ctx', name : names)) (ctx, []) ts
    (ctx'', e) <- withLocals ctx' ns ts $ \c -> genExp c (n - 1) t2
    let funl = [Symbol TSymbol Nothing lambda, listToCons (map (\n -> Symbol TSymbol Nothing n) ns), e]
    return (ctx'', listToCons funl)

  where
    genIf :: Gen (Ctx, SExp Type)
    genIf = do
      (ctx', c) <- genExp ctx (n `div` 3) TBool
      (ctx'', thn) <- genExp ctx' (n `div` 3) t
      (ctx''', els) <- genExp ctx'' (n `div` 3) t
      let exps = [Symbol TSymbol Nothing (Sym "if"), c, thn, els]
      return (ctx''', listToCons exps)

    genLet :: Gen (Ctx, SExp Type)
    genLet = do
      numbindings <- chooseInt (1, 5)
      let (ctx', names) = freshN ctx "local-var" numbindings -- generate numbindings fresh names
      lnts <- sequence $ replicate numbindings arbitrary -- generate arbitrary types for the let-variables
      (ctx'', es) <- genExps ctx' (n `div` numbindings) lnts -- generate an expression for each of the generated types
      (ctx''', e) <- withLocals ctx'' names lnts $ (\c -> genExp c (n - 1) t) -- with a scope extended with the let-bound stuff, generate the body
      let innerAssocs = zipWith (\n e -> listToCons [Symbol TSymbol Nothing n, e]) names es
          outerBindings = listToCons innerAssocs
          res = listToCons [Symbol TSymbol Nothing (Sym "let"), outerBindings, e] -- create the final S-expression
      return (ctx''', res)

    genSetvar :: Gen (Ctx, SExp Type)
    genSetvar = do
      let l = filter (\(i, Symbol _ (Just t') _,b) -> t' == t) $ reachableSymbols ctx
          gens = map (\(i, Symbol _ _ s,b) -> (i, return (s,b))) l
          lete = Symbol TSymbol Nothing (Sym "let")
          setq = Symbol TSymbol Nothing (Sym "setq")
      set <- elements [Symbol TSymbol Nothing (Sym "set")]
      if not (null gens)
        then do
          (se,b) <- frequency gens
          (ctx', e) <- genExp ctx (n - 1) t `suchThat` (\(_, e) -> not (elem se (free e)))
          let exps1 = [set, Quote TSymbol (Symbol TSymbol Nothing se), e]
              exps2 = [setq, Symbol TSymbol Nothing se, e]
          exps <- elements [exps1, exps2]
          return (ctx', listToCons exps)
        else do
          let (ctx', name) = fresh ctx "local-var"
          (ctx'', e1) <- genExp ctx' (n `div` 2) t
          (ctx''', e2) <- genExp ctx'' (n `div` 2) t
          let exps1 = [lete, listToCons [listToCons [Symbol TSymbol Nothing name, e1]], listToCons [set, Quote TSymbol (Symbol TSymbol Nothing name), e2]]
              exps2 = [lete, listToCons [listToCons [Symbol TSymbol Nothing name, e1]], listToCons [setq, Symbol TSymbol Nothing name, e2]]
          exps <- elements [exps1, exps2]
          return (ctx''', listToCons exps)

    genProgn :: Gen (Ctx, SExp Type)
    genProgn = do
      numexps <- chooseInt (1, n * 2) -- number of s-expressions. n is from the toplevel genexp, and is usually between 1 and 5
      types <- sequence $ replicate numexps arbitrary -- arbitrary types
      (ctx', es) <- genExps ctx (n `div` numexps) types
      (ctx'', e) <- genExp ctx' (n `div` numexps) t -- final expression, of the desired type

      let exps = Symbol TSymbol Nothing progn : es <> [e]
          exp = listToCons exps
      return (ctx'', exp)

    -- \| generate numeric expressions whose return type is t
    numGenerator :: Type -> Gen (Ctx, SExp Type)
    numGenerator t =
      frequency
        [ (1, genExp ctx (n - 1) t),
          (1, (,) ctx <$> genFunApp ctx (n - 1) t),
          (1, genIf),
          (1, genLet),
          (1, genSetvar),
          (2, genProgn),
          ( 2,
            do
              numparams <- chooseInt (1, 10)
              paramtypes <- sequence $ replicate numparams $ genNumericLTE t
              (ctx', es) <- genExps ctx (n `div` numparams) (t : paramtypes) -- at least one operand has to be of type t, to make sure result is of type t
              es' <- shuffle es
              f <- elements arithmeticOpInt -- shuffle the operands
              return (ctx', cons f (listToCons es))
          )
        ]

genFunApp :: Ctx -> Int -> Type -> Gen (SExp Type)
genFunApp ctx n d =
  let funs = filter (\(_, Symbol _ (Just t) _, b) -> isFun t && domain t == d) $ reachableSymbols ctx
      gens = map (\(i, s@(Symbol _ (Just t) _), b) -> (i, return (s, t))) funs -- TODO FIXME if we generate arbitrary expressions in the form of functions, we will need to handle the case where some functions might be quoted and some might not
   in if not (null funs)
        then genApp =<< frequency gens -- TODO maybe generate lambda here
        else genApp =<< genFunction ctx n d
  where
    genApp :: (SExp Type, Type) -> Gen (SExp Type)
    genApp (s, TFun params d) = do
      es <- (snd . unzip) <$> mapM (\t -> genExp ctx (n - 1) t) params
      let exps = s : es
          funapp = listToCons exps
      return funapp

-- | Generate a context, whose toplevel field will hold the final program. Right now, hardcodes
-- between (1,50] top level definitions. This, and many other values, should be configurable
genCtx :: Gen Ctx
genCtx = do
  n <- chooseInt (1, 50)
  ctx' <-
    foldM
      ( \ctx _ ->
          oneof
            [ genEnvConstant ctx,
              genEnvFunction ctx
            ]
      )
      newCtx
      [0 .. n]
  return $ ctx' {toplevel = reverse (toplevel ctx')}

instance Arbitrary Ctx where
  arbitrary = genCtx
  shrink = shrinkCtx []
