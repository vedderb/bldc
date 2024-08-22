{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Property where

import Data.Bits
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Word
import Data.IORef
import Data.Either

import Driver
import Syntax
import Oracles
import System.Directory
import System.Process
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Unsafe.Coerce
import Control.Monad (foldM)

-- Create unit tests

subdir :: String
subdir = "Generated"

pathToUnitTests :: String
pathToUnitTests = "tests/Regression/" <> subdir <> "/"

-- Counterexample plotting

prettyCounterexample :: (Environment,SExp Type, RunResult) -> String
prettyCounterexample (env0,e,r) =
  "In env: \n" ++
  (prettyEnv env0) ++ "\n" ++ 
  "===>\n" ++
  (prettyExp e) ++ " = " ++ (prettyExp res) ++
  "<===\n" ++
  (prettyEnv env)
  where (res, env) = fromRight (nil, M.empty) r

counterexampleList :: Monad m => [(Environment, SExp Type, RunResult)] -> PropertyM m ()
counterexampleList es = do 
  monitor $
    counterexample $
    "Counterexample: \n" ++
    if (all isRight (map (\(x,y,z) -> z)  es))
    then concat $ intersperse "\n" (map prettyCounterexample es)
    else "LEFTS"
  


-- | Generate a random module name to use for the generated unit test
randomModuleName :: IO String
randomModuleName = do
  g <- initStdGen
  let (a, _) = random g :: (Int, StdGen)
      mname = "Arb" <> (show $ abs a) <> "Spec"
  b <- doesFileExist (pathToUnitTests <> mname <> ".hs")
  if b
    then randomModuleName
    else return mname

-- | Given an environment and a module name, creates a rerunnable unit test that
-- reruns the test for the given environment
envToUnitTest :: Environment -> String -> String
envToUnitTest env modulename =
  unlines
    [ "module " <> subdir <> "." <> modulename <> " where",
      "import qualified Data.Map as M",
      "import SExpGen",
      "import Test.Hspec as H",
      "",
      "env :: Environment",
      "env = M." <> show env,
      "",
      "spec :: H.Spec",
      "spec = specify \"" <> modulename <> "\" $ (decEnv (encEnv env)) == env"
    ]

createUnitTest :: Environment -> IO ()
createUnitTest env = do
  mname <- randomModuleName
  writeFile (pathToUnitTests <> mname <> ".hs") $ envToUnitTest env mname
  putStrLn $ "wrote generated unit-test to " <> pathToUnitTests <> mname


-- * Properties

prop_env :: Ctx -> Property
prop_env ctx = monadicIO $ do
  let env = toplevelToEnv ctx
      env' = decEnv $ encEnv env

  monitor $
    whenFail $
      do
        putStrLn $ "Environment diff: " ++ show (envDiff env env')
        createUnitTest env

  assert $ env == env'

prop_env_lbm :: Ctx -> Property
prop_env_lbm ctx = monadicIO $ do
  let env = toplevelToEnv ctx

  run $ storeEnv "e1.env" env
  run $ callCommand "lbm --load_env=e1.env --store_env=e2.env --terminate"
  env' <- run $ loadEnv "e2.env"

  monitor $
    whenFail $
      do
        putStrLn $ "Environment diff: " ++ show (envDiff env env')
        callCommand "xxd e1.env > e1.hex"
        callCommand "xxd e2.env > e2.hex"
        callCommand "diff e1.hex e2.hex"
        createUnitTest env

  assert $ env == env'

prop_nil_program :: Ctx -> Property
prop_nil_program ctx = monadicIO $ do
  let env = toplevelToEnv ctx

      prg = nil

  (Right (res, env')) <- run $ compileAndRun env prg

  monitor $
    whenFail $
      do
        putStrLn $ "Environment diff: " ++ show (envDiff env env')
        createUnitTest env

  assert $ env == env' && res == prg

data NumericExpressions = NumericExpressions Ctx [SExp Type]

instance Show NumericExpressions where
  show (NumericExpressions ctx es) = "===== Ctx =====\n" <> show ctx <> "\n===== Env =====\n" <> prettyCtx ctx <> "\n===== Exps =====\n--- " <> intercalate "\n--- " (map prettyExp es)

instance Arbitrary NumericExpressions where
  arbitrary :: Gen NumericExpressions
  arbitrary = do
    ctx <- genCtx
    numexps <- chooseInt (1,10)
    ts <- sequence $ replicate numexps genNumericType
    s <- chooseInt (1, 5)
    (ctx', es) <- genExps ctx s ts
    return $ NumericExpressions ctx es

  shrink :: NumericExpressions -> [NumericExpressions]
  shrink (NumericExpressions ctx es) =
    [ NumericExpressions (ctx' { toplevel = reverse (toplevel ctx') }) es
      | ctx' <- shrinkCtx (nub $ concatMap free es) (ctx { toplevel = reverse (toplevel ctx)})
    ]
      <> [ NumericExpressions ctx es'
           | es' <- shrinkList shrinkSExp es,
             not (null es')
         ]

-- | Call this to turn a context into an environment we can load
-- the intger number is meant to be given to compileandrun
toEnv :: Ctx -> IO Environment
toEnv ctx = return $ toplevelToEnv ctx

prettyEnv :: Environment -> String
prettyEnv env | M.null env = "<empty>"
prettyEnv env = unlines $ map (\(n,e) -> concat [show n, " : ", prettyExp e]) $ M.toList env

individualAdds :: Environment -> [SExp Type] -> Maybe (SExp Type) -> PropertyM IO (Either String (SExp Type, Environment))
individualAdds env es acc = foldOverResults env es acc addExps

individualSubs :: Environment -> [SExp Type] -> Maybe (SExp Type) -> PropertyM IO (Either String (SExp Type, Environment))
individualSubs env [e] _ = do
  r <- run $ compileAndRun env e
  Right (res, env') <- guardAgainstError r
  return $ Right (negateExp (toSExp env' res), env')
individualSubs env es acc = foldOverResults env es acc subExps

prop_add :: NumericExpressions -> Property
prop_add (NumericExpressions ctx es) = monadicIO $ do
  env <- run $ toEnv ctx

  r1 <- individualAdds env es Nothing
  r2' <- run $ compileAndRun env (Add (listToCons es))
  r2 <- guardAgainstError r2'
  let Right (res, env')   = r1 -- <- individualAdds env es Nothing
  let Right (res3, env'') = r2 -- <- run $ compileAndRun env (Add (listToCons es))

  monitor $ whenFail $ do
    putStrLn "====="
    putStrLn "expected environment:"
    putStrLn $ prettyEnv env'
    putStrLn "actual environment:"
    putStrLn $ prettyEnv env''
    putStrLn "====="
    putStrLn "Individually evaluated expressions:"
    putStrLn $ init $ unlines $ zipWith (\e i -> "e" <> show i <> " : " <> prettyExp e) es [1..]
    putStrLn "Should together produce the same result as:"
    putStrLn $ "(add e1 e2) = " <> prettyExp (Add (listToCons es))
    putStrLn "====="
    putStrLn $ "expected result : " <> show res
    putStrLn $ "actual result   : " <> show res3
    putStrLn "====="
    storeEnv "env.env" env

  assert $ env' == env'' && res == res3

prop_sub :: NumericExpressions -> Property
prop_sub (NumericExpressions ctx es) = monadicIO $ do
  env <- run $ toEnv ctx

  r1 <- individualSubs env es Nothing
  r2' <- run $ compileAndRun env (Sub (listToCons es))
  r2 <- guardAgainstError r2'
  let Right (res, env')   = r1 -- <- individualAdds env es Nothing
  let Right (res3, env'') = r2 -- <- run $ compileAndRun env (Add (listToCons es))

  monitor $ whenFail $ do
    putStrLn "====="
    putStrLn "expected environment:"
    putStrLn $ prettyEnv env'
    putStrLn "actual environment:"
    putStrLn $ prettyEnv env''
    putStrLn "====="
    putStrLn "Individually evaluated expressions:"
    putStrLn $ init $ unlines $ zipWith (\e i -> "e" <> show i <> " : " <> prettyExp e) es [1..]
    putStrLn "Should together produce the same result as:"
    putStrLn $ "(sub ...) = " <> prettyExp (Sub (listToCons es))
    putStrLn "====="
    putStrLn $ "expected result : " <> show res
    putStrLn $ "actual result   : " <> show res3
    putStrLn "====="
    storeEnv "envsub.env" env

  assert $ env' == env'' && res == res3

data ArbitraryExpressions = ArbitraryExpressions Ctx [SExp Type]

instance Show ArbitraryExpressions where
  show (ArbitraryExpressions ctx es) = "===== Ctx =====\n" <> show ctx <> "\n===== Env =====\n" <> prettyCtx ctx <> "\n===== Exps =====\n---" <> intercalate "\n---" (map prettyExp es)

instance Arbitrary ArbitraryExpressions where
  arbitrary :: Gen ArbitraryExpressions
  arbitrary = do
    ctx <- genCtx
    numexps <- chooseInt (1,3)
    ts <- sequence $ replicate numexps arbitrary
    s <- chooseInt (1, 5)
    (ctx', es) <- genExps ctx s ts
    return $ ArbitraryExpressions ctx' es

  shrink :: ArbitraryExpressions -> [ArbitraryExpressions]
  shrink (ArbitraryExpressions ctx es) =
    [ ArbitraryExpressions (ctx' { toplevel = reverse (toplevel ctx') }) es
      | ctx' <- shrinkCtx (nub $ concatMap free es) (ctx { toplevel = reverse (toplevel ctx)})
    ]
      <> [ ArbitraryExpressions ctx es'
           | es' <- shrinkList shrinkSExp es,
             not (null es')
         ]

data ArbitrarySingleExpressions = ArbitrarySingleExpressions Ctx (SExp Type)

instance Show ArbitrarySingleExpressions where
  show (ArbitrarySingleExpressions ctx e) = "===== Ctx =====\n" <> show ctx <> "\n===== Env =====\n" <> prettyCtx ctx <> "\n===== Exp =====\n---" <> prettyExp e

instance Arbitrary ArbitrarySingleExpressions where
  arbitrary :: Gen ArbitrarySingleExpressions
  arbitrary = do
    ctx <- genCtx
    t <- arbitrary
    s <- chooseInt (1, 5)
    (ctx', e) <- genExp ctx s t
    return $ ArbitrarySingleExpressions ctx' e

  shrink :: ArbitrarySingleExpressions -> [ArbitrarySingleExpressions]
  shrink (ArbitrarySingleExpressions ctx e) =
    [ ArbitrarySingleExpressions (ctx' { toplevel = reverse (toplevel ctx') }) e
      | ctx' <- shrinkCtx (free e) (ctx { toplevel = reverse (toplevel ctx)})
    ]
      <> [ ArbitrarySingleExpressions ctx e'
           | e' <- shrinkSExp e
         ]

prop_not :: ArbitrarySingleExpressions -> Property
prop_not (ArbitrarySingleExpressions ctx e) = monadicIO $ do
  env <- run $ toEnv ctx
  r1 <- run $ compileAndRun env e
  guardAgainstError r1
  r2 <- run $ compileAndRun env (Not e)
  guardAgainstError r2

  let Right (res1, env') = r1
      Right (res2, env'') = r2
  
  let expected = case res1 of
        Symbol t1 mt2 (Sym "nil") -> Symbol t1 mt2 (Sym "t")
        _ -> Symbol TSymbol Nothing (Sym "nil")

  monitor $ whenFail $ do
    putStrLn "====="
    putStrLn "expected environment:"
    putStrLn $ prettyEnv env'
    putStrLn "actual environment:"
    putStrLn $ prettyEnv env''
    putStrLn "====="
    putStrLn "Individually evaluated expressions:"
    putStrLn $ "e = " <> prettyExp e
    putStrLn "Inverted, this should produce the same result as:"
    putStrLn $ "(not ...) = " <> prettyExp (Not e)
    putStrLn "====="
    putStrLn $ "expected result : " <> show expected
    putStrLn $ "actual result   : " <> show res2
    putStrLn "====="
    storeEnv "envnot.env" env

  assert $ expected == res2 && env' == env''

-- prop_progn :: ArbitraryExpressions -> Property
-- prop_progn (ArbitraryExpressions ctx es) = monadicIO $ do
--   env <- run $ toEnv ctx
--   r1 <- foldOverResults env es Nothing (\_ e2 -> e2)
--   r2 <- run $ compileAndRun env (Progn (listToCons es))
--   guardAgainstError r2

--   let Right (res, env')   = r1
--   let Right (res', env'') = r2

--   monitor $ whenFail $ do
--     putStrLn "====="
--     putStrLn "expected environment:"
--     putStrLn $ prettyEnv env'
--     putStrLn "actual environment:"
--     putStrLn $ prettyEnv env''
--     putStrLn "====="
--     putStrLn "Individually evaluated expressions:"
--     putStrLn $ init $ unlines $ zipWith (\e i -> "e" <> show i <> " : " <> prettyExp e) es [1..]
--     putStrLn "Should together produce the same result as:"
--     putStrLn $ "(progn ...) = " <> prettyExp (Progn (listToCons es))
--     putStrLn "====="
--     putStrLn $ "expected result : " <> show res
--     putStrLn $ "actual result   : " <> show res'
--     putStrLn "====="
--     storeEnv "envprogn.env" env    

--   assert $ res == res' && env' == env''

prop_and :: ArbitraryExpressions -> Property
prop_and (ArbitraryExpressions ctx es) = monadicIO $ do
  env <- run $ toEnv ctx
  Right (r, env') <- foldUntil env es test
  
  x <- run $ compileAndRun env (Syntax.And (listToCons es))
  guardAgainstError x
  let Right (r', env'') = x

  monitor $ whenFail $ do
    putStrLn "====="
    putStrLn "expected environment:"
    putStrLn $ prettyEnv env'
    putStrLn "actual environment:"
    putStrLn $ prettyEnv env''
    putStrLn "====="
    putStrLn "Individually evaluated expressions:"
    putStrLn $ init $ unlines $ zipWith (\e i -> "e" <> show i <> " : " <> prettyExp e) es [1..]
    putStrLn "Should together produce the same result as:"
    putStrLn $ "(and ...) = " <> prettyExp (Syntax.And (listToCons es))
    putStrLn "====="
    putStrLn $ "expected result : " <> show r
    putStrLn $ "actual result   : " <> show r'
    putStrLn "====="
    storeEnv "envprogn.env" env    

  assert $ r == r' && env' == env''
  where
    test :: SExp Type -> Bool
    test (Symbol _ _ (Sym "nil")) = True
    test _ = False

prop_or :: ArbitraryExpressions -> Property
prop_or (ArbitraryExpressions ctx es) = monadicIO $ do
  env <- run $ toEnv ctx
  Right (r, env') <- foldUntil env es test
  x <- run $ compileAndRun env (Or (listToCons es))
  guardAgainstError x
  let Right (r', env'') = x

  monitor $ whenFail $ do
    putStrLn "====="
    putStrLn "expected environment:"
    putStrLn $ prettyEnv env'
    putStrLn "actual environment:"
    putStrLn $ prettyEnv env''
    putStrLn "====="
    putStrLn "Individually evaluated expressions:"
    putStrLn $ init $ unlines $ zipWith (\e i -> "e" <> show i <> " : " <> prettyExp e) es [1..]
    putStrLn "Should together produce the same result as:"
    putStrLn $ "(or ...) = " <> prettyExp (Or (listToCons es))
    putStrLn "====="
    putStrLn $ "expected result : " <> show r
    putStrLn $ "actual result   : " <> show r'
    putStrLn "====="
    storeEnv "envprogn.env" env    

  assert $ r == r' && env' == env''
  where
    test :: SExp Type -> Bool
    test (Symbol _ _ (Sym "nil")) = False
    test _ = True

prop_eq :: ArbitraryExpressions -> Property
prop_eq (ArbitraryExpressions ctx es) = monadicIO $ do
  env <- run $ toEnv ctx
  r1 <- foldUntilNotEqual env es []
  r2 <- run $ compileAndRun env (SAEq (listToCons es))
  guardAgainstError r2

  let Right (res, env')   = r1
  let Right (res', env'') = r2

  monitor $ whenFail $ do
    putStrLn "====="
    putStrLn "expected environment:"
    putStrLn $ prettyEnv env'
    putStrLn "actual environment:"
    putStrLn $ prettyEnv env''
    putStrLn "====="
    putStrLn "Individually evaluated expressions:"
    putStrLn $ init $ unlines $ zipWith (\e i -> "e" <> show i <> " : " <> prettyExp e) es [1..]
    putStrLn "Should together produce the same result as:"
    putStrLn $ "(eq ...) = " <> prettyExp (SAEq (listToCons es))
    putStrLn "====="
    putStrLn $ "expected result : " <> show res
    putStrLn $ "actual result   : " <> show res'
    putStrLn "====="
    storeEnv "enveq.env" env
    mapM_ (\(i, e) -> do
      Right (r, _) <- compileAndRun M.empty e
      writeFile ("e" <> show i <> ".res") (show r)) (zip [1..] es)

  assert $ res == res' && env' == env''

prop_not_eq :: ArbitraryExpressions -> Property
prop_not_eq (ArbitraryExpressions ctx es) = monadicIO $ do
  env <- run $ toEnv ctx
  r1 <- run $ compileAndRun env (Not (SAEq (listToCons es)))
  guardAgainstError r1
  r2 <- run $ compileAndRun env (SANEq (listToCons es))
  guardAgainstError r2

  let Right (res, env')   = r1
  let Right (res', env'') = r2

  monitor $ whenFail $ do
    putStrLn "====="
    putStrLn "expected environment:"
    putStrLn $ prettyEnv env'
    putStrLn "actual environment:"
    putStrLn $ prettyEnv env''
    putStrLn "====="
    putStrLn $ "e1 : " <> prettyExp (Not (SAEq (listToCons es)))
    putStrLn $ "e2 : " <> prettyExp (SANEq (listToCons es))
    putStrLn "====="
    putStrLn $ "expected result : " <> show res
    putStrLn $ "actual result   : " <> show res'
    putStrLn "====="
    storeEnv "envneq.env" env
    mapM_ (\(i, e) -> do
      Right (r, _) <- compileAndRun M.empty e
      writeFile ("e" <> show i <> ".res") (show r)) (zip [1..] es)

  assert $ res == res' && env' == env''

prop_num_eq :: NumericExpressions -> Property
prop_num_eq (NumericExpressions ctx es) = monadicIO $ do
  env <- run $ toEnv ctx
  r1 <- foldUntilNotEqualNumeric env es []
  r2 <- run $ compileAndRun env (SEq (listToCons es))
  guardAgainstError r2

  let Right (res, env')   = r1
  let Right (res', env'') = r2

  monitor $ whenFail $ do
    putStrLn "====="
    putStrLn "expected environment:"
    putStrLn $ prettyEnv env'
    putStrLn "actual environment:"
    putStrLn $ prettyEnv env''
    putStrLn "====="
    putStrLn "Individually evaluated expressions:"
    putStrLn $ init $ unlines $ zipWith (\e i -> "e" <> show i <> " : " <> prettyExp e) es [1..]
    putStrLn "Should together produce the same result as:"
    putStrLn $ "(= ...) = " <> prettyExp (SEq (listToCons es))
    putStrLn "====="
    putStrLn $ "expected result : " <> show res
    putStrLn $ "actual result   : " <> show res'
    putStrLn "====="
    storeEnv "enveq.env" env
    mapM_ (\(i, e) -> do
      Right (r, _) <- compileAndRun M.empty e
      writeFile ("e" <> show i <> ".res") (show r)) (zip [1..] es)

  assert $ res == res' && env' == env''

prop_not_num_eq :: NumericExpressions -> Property
prop_not_num_eq (NumericExpressions ctx es) = monadicIO $ do
  env <- run $ toEnv ctx
  r1 <- run $ compileAndRun env (Not (SEq (listToCons es)))
  guardAgainstError r1
  r2 <- run $ compileAndRun env (SNEq (listToCons es))
  guardAgainstError r2

  let Right (res, env')   = r1
  let Right (res', env'') = r2

  monitor $ whenFail $ do
    putStrLn "====="
    putStrLn "expected environment:"
    putStrLn $ prettyEnv env'
    putStrLn "actual environment:"
    putStrLn $ prettyEnv env''
    putStrLn "====="
    putStrLn $ "e1 : " <> prettyExp (Not (SAEq (listToCons es)))
    putStrLn $ "e2 : " <> prettyExp (SANEq (listToCons es))
    putStrLn "====="
    putStrLn $ "expected result : " <> show res
    putStrLn $ "actual result   : " <> show res'
    putStrLn "====="
    storeEnv "envneq.env" env
    mapM_ (\(i, e) -> do
      Right (r, _) <- compileAndRun M.empty e
      writeFile ("e" <> show i <> ".res") (show r)) (zip [1..] es)

  assert $ res == res' && env' == env''

prop_mod :: NumericExpressions -> Property
prop_mod (NumericExpressions ctx es) = length es >= 2 ==> monadicIO $ do
  let e1:e2:_ = es
  env <- run $ toEnv ctx

  r1 <- run $ compileAndRun env e1
  guardAgainstError r1
  let Right (op1, env') = r1

  r2 <- run $ compileAndRun env' e2
  guardAgainstError r2
  let Right (op2, env'') = r2

  let expected = if isZero op2
        then Symbol TSymbol Nothing (Sym "division_by_zero")
        else modExps op1 op2
  
  r3 <- run $ compileAndRun env (listToCons [Symbol TSymbol Nothing (Sym "mod"), e1, e2])

  case r3 of
    Left _ -> assert False
    Right (res3, env''') -> do
        monitor $ whenFail $ do
          putStrLn $ "expected: " <> show expected
          putStrLn $ "lispBM:   " <> show res3
        assert $ env'' == env''' && res3 == expected

data IfExpression = IfExpression Ctx (SExp Type) (SExp Type) (SExp Type)

instance Show IfExpression where
  show (IfExpression ctx c thn els) =
    "===== Ctx =====\n" <>show ctx <>
    "\n===== Env =====\n" <> prettyCtx ctx <>
    "\n===== Exps =====\n---c = " <> prettyExp c <> "\n---thn = " <> prettyExp thn <> "\n---els = " <> prettyExp els

instance Arbitrary IfExpression where
  arbitrary :: Gen IfExpression
  arbitrary = do
    ctx <- genCtx
    t <- arbitrary
    t2 <- arbitrary
    s <- chooseInt (1, 5)
    (ctx', es) <- genExps ctx s [t,t2,t2]
    let [c,thn,els] = es
    return $ IfExpression ctx' c thn els

  shrink :: IfExpression -> [IfExpression]
  shrink (IfExpression ctx c thn els) =
    [ IfExpression (ctx' { toplevel = reverse (toplevel ctx') }) c thn els
      | ctx' <- shrinkCtx (free c `union` free thn `union` free els) (ctx { toplevel = reverse (toplevel ctx)})
    ]
      <> [ IfExpression ctx c' thn els
           | c' <- shrinkSExp c
         ]
      <> [ IfExpression ctx c thn' els
           | thn' <- shrinkSExp thn
         ]
      <> [ IfExpression ctx c thn els'
           | els' <- shrinkSExp els
         ]

prop_if :: IfExpression -> Property
prop_if (IfExpression ctx c thn els) = monadicIO $ do
  env <- run $ toEnv ctx
  r <- run $ compileAndRun env c
  guardAgainstError r
  let Right (s,env') = r

  let next = case s of
        Symbol _ _ (Sym "nil") -> els
        _ -> thn
  
  r2 <- run $ compileAndRun env' next
  guardAgainstError r2

  r3 <- run $ compileAndRun env (If c thn els)
  guardAgainstError r3

  let Right (res1, env'') = r2
      Right (res2, env''') = r3
  
  assert $ res1 == res2 && env'' == env'''

data PropGCProgn = PropGCProgn Ctx [SExp Type]

prop_num_eq_is_eq :: NumericExpressions -> Property
prop_num_eq_is_eq (NumericExpressions ctx es) = monadicIO $ do
  env <- run $ toEnv ctx
  r1 <- run $ compileAndRun env (SAEq (listToCons es))
  r2 <- run $ compileAndRun env (SEq (listToCons es))
  guardAgainstError r2

  let Right (res, env')   = r1 -- eq
  let Right (res', env'') = r2 -- =

  monitor $ whenFail $ do
    putStrLn "====="
    putStrLn "expected environment:"
    putStrLn $ prettyEnv env'
    putStrLn "actual environment:"
    putStrLn $ prettyEnv env''
    putStrLn "====="
    putStrLn "Individually evaluated expressions:"
    putStrLn $ init $ unlines $ zipWith (\e i -> "e" <> show i <> " : " <> prettyExp e) es [1..]
    putStrLn "Should together produce the same result as:"
    putStrLn $ prettyExp (SAEq (listToCons es)) <> " = " <> prettyExp (SEq (listToCons es))
    putStrLn "====="
    putStrLn $ "eq said : " <> show res
    putStrLn $ "= said  : " <> show res'
    putStrLn "====="
    storeEnv "enveqeq.env" env
    mapM_ (\(i, e) -> do
      Right (r, _) <- compileAndRun M.empty e
      writeFile ("e" <> show i <> ".res") (show r)) (zip [1..] es)

  assert $ res == res' && env' == env''

instance Show PropGCProgn where
  show (PropGCProgn ctx es) = "===== Env =====\n" <> prettyCtx ctx <> "\n===== Exps =====\n---" <> intercalate "\n---" (map prettyExp es)

instance Arbitrary PropGCProgn where
  arbitrary :: Gen PropGCProgn
  arbitrary = do
    ctx <- genCtx
    i1 <- chooseInt(1,5)
    i2 <- chooseInt(1,5)
    t1 <- arbitrary
    t2 <- arbitrary
    (ctx', e1) <- genExp ctx i1 t1
    (ctx'', e2) <- genExp ctx i2 t2
    return $ PropGCProgn ctx'' [e1,e2]

  -- shrink :: TCAdd -> [TCAdd]
  -- shrink (TCAdd ctx es) =
  --   [ TCAdd ctx' es
  --     | ctx' <- shrinkCtx (foldl union [] $ map free es) ctx
  --   ]
  --     <> [ TCAdd ctx es'
  --          | es' <- shrinkList shrinkSExp es, -- applyElementWise shrinkSExp es,
  --            not (null es')
  --        ]

prop_gc_progn (PropGCProgn ctx [e1, e2]) =  monadicIO $ do
  let env = toplevelToEnv ctx
      prg1 = lispProgn [e1, gc, e2]
      prg2 = lispProgn [e1, e2]
             
  r_1@(Right (r1, env1)) <- run $ compileAndRun env prg1
  guardAgainstError r_1
  r_2@(Right (r2, env2)) <- run $ compileAndRun env prg2

  counterexampleList [(env,prg1, r_1), (env,prg2, r_2)];
  
  monitor $
    whenFail $
    do
      putStrLn $ "Environment diff: " ++ show (envDiff env1 env2)
      createUnitTest env
        
  assert $ env1 == env2 && r1 == r2


randExp :: Ctx -> Gen (Ctx, SExp Type)
randExp ctx = do
  i <- chooseInt(1, 5)
  j <- chooseInt(1, 10)
  t  <- genType j
  genExp ctx i t


randomExp :: Gen (Ctx, SExp Type)
randomExp = do
  ctx <- genCtx
  randExp ctx
  
shrinkRandomExp :: (Ctx, SExp Type) -> [(Ctx, SExp Type)]
shrinkRandomExp (ctx, e) =
  [(ctx', e) | ctx' <- shrinkCtx (free e) ctx] -- context shrinker has to know which free symbols
                                               -- the expression uses, so the shrinker accepts that
                                               -- as a parameter
    <> [(ctx, e') | e' <- shrinkSExp e]

-- TODO Can this situation be improved with some type classes ? 
randomExpPair :: Gen (Ctx, (SExp Type, SExp Type))
randomExpPair = do
  ctx <- genCtx
  (ctx', e1) <- randExp ctx 
  (ctx'', e2) <- randExp ctx'
  return (ctx'' , (e1, e2))

shrinkRandomExpPair :: (Ctx, (SExp Type,SExp Type)) -> [(Ctx, (SExp Type,SExp Type))]
shrinkRandomExpPair (ctx, (e1, e2)) =
  [(ctx', (e1, e2)) | ctx' <- shrinkCtx (free e1 ++ free e2) ctx] <> [(ctx, (e1', e2')) | e1' <- shrinkSExp e1, e2' <- shrinkSExp e2]
  
  
randomExpList :: Int -> Int -> Gen (Ctx, [SExp Type])
randomExpList s f = do
  ctx <- genCtx
  n <- chooseInt(s,f)
  do_it ctx n
    where
      do_it :: Ctx -> Int -> Gen (Ctx, [SExp Type])
      do_it ctx 0 = return (ctx, [])
      do_it ctx n = do
        (ctx', e) <- randExp ctx
        (ctx'', es) <- do_it ctx' (n - 1)
        return (ctx'', e:es)
        
shrinkRandomExpList :: (Ctx, [SExp Type]) -> [(Ctx, [SExp Type])]
shrinkRandomExpList (ctx, es) =
  [(ctx', es) | ctx' <- shrinkCtx (concatMap free es) ctx] <> [(ctx, es') | es' <- map shrinkSExp es] -- Robert Fix it, please.


-- ------------------------------------------------------------
-- Prop_unflatten_flatten

prop_unflatten_flatten :: Property
prop_unflatten_flatten =
  forAllShrink randomExp shrinkRandomExp $ \(ctx, exp) -> monadicIO $ do
  let env1 = toplevelToEnv ctx
      prg = (cons unflatten (cons (cons flatten (cons exp nil)) nil))

  (Right (r1, env2)) <- run $ compileAndRun env1 prg
  (Right (r2, env3)) <- run $ compileAndRun env1 exp

  monitor $
    whenFail $
    do
      putStrLn $ "Environment diff: " ++ show (envDiff env2 env3)
      putStrLn $ "prg: " ++ prettyExp prg
      createUnitTest env1
        
  assert $ env2 == env3 && r1 == r2

-- ------------------------------------------------------------
-- Prop_if_true

prop_if_true :: Property
prop_if_true = forAllShrink randomExpPair shrinkRandomExpPair $ \(ctx, (e1, e2)) -> monadicIO $ do
  let env1 = toplevelToEnv ctx
      prg  = lispIf (mkSymbol "t") e1 e2

  (Right (r1, env2)) <- run $ compileAndRun env1 prg
  (Right (r2, env3)) <- run $ compileAndRun env1 e1

  -- replace t with random expression evaluating to true, and this property should
  -- not hold.
  
  monitor $
    whenFail $
    do
      putStrLn $ "Environment diff: " ++ show (envDiff env2 env3)
      putStrLn $ "prg: " ++ prettyExp prg
      createUnitTest env1
  
  assert $ env2 == env3 && r1 == r2

-- ------------------------------------------------------------
-- Prop_if_false

prop_if_false :: Property
prop_if_false = forAllShrink randomExpPair shrinkRandomExpPair $ \(ctx, (e1, e2)) -> monadicIO $ do
  let env1 = toplevelToEnv ctx
      prg  = lispIf (mkSymbol "nil") e1 e2

  (Right (r1, env2)) <- run $ compileAndRun env1 prg
  (Right (r2, env3)) <- run $ compileAndRun env1 e2

  -- replace t with random expression evaluating to true, and this property should
  -- not hold.
  
  monitor $
    whenFail $
    do
      putStrLn $ "Environment diff: " ++ show (envDiff env2 env3)
      putStrLn $ "prg: " ++ prettyExp prg
      createUnitTest env1
  
  assert $ env2 == env3 && r1 == r2
  

-- ------------------------------------------------------------
-- Prop_cond

prop_cond :: Property
prop_cond = forAllShrink (randomExpList 2 10) shrinkRandomExpList $ \(ctx, es) -> monadicIO $ do
  let env1 = toplevelToEnv ctx
      len  = length es
      h1   = head es
      h2   = head (tail es)
      es'  = drop 2 es
      l1   = take (len `div` 2) es'
      l2   = drop (len `div` 2) es'
      prg1 = lispCond (h1:l1) (h2:l2)
      prg2 = lispIf h1 h2 (lispCond l1 l2)

  (Right (r1, env2)) <- run $ compileAndRun env1 prg1
  (Right (r2, env3)) <- run $ compileAndRun env1 prg2

  monitor $
    whenFail $
    do
      putStrLn $ "Environment diff: " ++ show (envDiff env2 env3)
      putStrLn $ "prg: " ++ prettyExp prg1
      createUnitTest env1
  
  assert $ env2 == env3 && r1 == r2
  

prop_eval_quote :: Property
prop_eval_quote =
  forAllShrink randomExp shrinkRandomExp $ \(ctx, exp) -> monadicIO $ do
  let env1 = toplevelToEnv ctx
      prg1 = lispEval (lispQuote exp)
      prg2 = exp
        
  (Right (r1, env2)) <- run $ compileAndRun env1 prg1
  (Right (r2, env3)) <- run $ compileAndRun env1 prg2

  monitor $
    whenFail $
    do
      putStrLn $ "Environment diff: " ++ show (envDiff env2 env3)
      putStrLn $ "prg1: " ++ prettyExp prg1
      putStrLn $ "prg2: " ++ prettyExp prg2
      createUnitTest env1
        
  assert $ env2 == env3 && r1 == r2


-- ------------------------------------------------------------
-- Prop_gc_2

prop_gc_2 :: Property
prop_gc_2 = forAllShrink (randomExpList 2 2000) shrinkRandomExpList $ \(ctx, es) -> monadicIO $ do
  let env1 = toplevelToEnv ctx
      prg1 = lispProgn (intersperse gc es)
      prg2 = lispProgn es

  (Right (r1, env2)) <- run $ compileAndRun env1 prg1
  (Right (r2, env3)) <- run $ compileAndRun env1 prg2

  monitor $
    whenFail $
    do
      putStrLn $ "Environment diff: " ++ show (envDiff env2 env3)
      putStrLn $ "prg1: " ++ prettyExp prg1
      putStrLn $ "prg2: " ++ prettyExp prg2
      createUnitTest env1
     
  assert $ env2 == env3 && r1 == r2
      
-- ------------------------------------------------------------
-- Prop_progn_step


-- TODO: I think that the random list of exps shrinks to less then enough elements...
-- Or is incorrectly generated that way. 
prop_progn_step :: Property
prop_progn_step = forAllShrink (randomExpList 3 20) shrinkRandomExpList $ \(ctx, es) -> monadicIO $ do
  if (length es < 3)
    then discard
    else return () 
  let env1 = toplevelToEnv ctx
      h = head es;
      t = tail es;
      prg1 = lispProgn es
      prg2_a = h
      prg2_b = lispProgn t

  -- if the es program does not error out, then neither should the chopped up progn.
  r_1@(Right (r1, env2)) <- run $ compileAndRun env1 prg1
  r_2a@(Right (r2_a, env3_a)) <- run $ compileAndRun env1 prg2_a
  r_2b@(Right (r2_b, env3_b)) <- run $ compileAndRun env3_a prg2_b

  counterexampleList [(env1,prg1, r_1), (env1,prg2_a, r_2a), (env3_a,prg2_b, r_2b)]

  monitor $
    whenFail $
    do
      putStrLn $ "===================================================="
      putStrLn $ "Environment diff: " ++ show (envDiff env2 env3_b)
      putStrLn $ "" 
      putStrLn $ "prg1: " ++ prettyExp prg1
      putStrLn $ "" 
      putStrLn $ "prg2_a: " ++ prettyExp prg2_a
      putStrLn $ "" 
      putStrLn $ "prg2_b: " ++ prettyExp prg2_b
      putStrLn $ ""
      putStrLn $ "r1: " ++ prettyExp r1
      putStrLn $ ""
      putStrLn $ "r2_a: " ++ prettyExp r2_a
      putStrLn $ ""
      putStrLn $ "r2_b: " ++ prettyExp r2_b
      createUnitTest env1

  -- If there is an error, then the same error should happen in prg2_a or prg2_b
  if (isLBMException r_1) 
    then assert $ (r1 == r2_b) || (r1 == r2_a)
    else assert $ env2 == env3_b && r1 == r2_b

-- | New addition relations

prop_add_single :: NumericExpressions -> Property
prop_add_single (NumericExpressions ctx (e:_)) = monadicIO $ do
  env <- run $ toEnv ctx

  r1 <- run $ compileAndRun env e
  guardAgainstError r1

  r2 <- run $ compileAndRun env (Add (listToCons [e]))
  guardAgainstError r2

  counterexampleList [(env,e,r1), (env, (Add (listToCons [e])),r2)]

  monitor $
    whenFail $
      do -- I want to add this info to the log 
        putStrLn $ show e
        putStrLn $ show (Add e)

 
  assert $ r1 == r2

prop_add_inductive :: NumericExpressions -> Property
prop_add_inductive (NumericExpressions ctx exps) =
  length exps >= 2 ==> monadicIO $ do -- TODO
    let en = last exps
        es = init exps --e1:es = exps
    env <- run $ toEnv ctx

    r1 <- run $ compileAndRun env (Add (listToCons es))
    guardAgainstError r1
    let Right (res1, env') = r1

    r2 <- run $ compileAndRun env' en
    guardAgainstError r2
    let Right (res2, env'') = r2
        expected = addExps res1 res2
  
    r3 <- run $ compileAndRun env (Add (listToCons (es <> [en])))
    guardAgainstError r3
    let Right (res3, env''') = r3

    assert $ expected == res3 && env'' == env'''

-- * From the ground up

-- Define s e
-- Lambda params body
-- Closure params body env
-- Progn exps
-- Var s e <-- not yet a pattern synonym
-- Let binds e

-- Set s e
-- Setq s e
-- Add ops
-- Sub ops
-- Mul ops
-- SEq ops
-- SNEq ops
-- SAEq ops
-- SANEq ops
-- SLE ops
-- SLT ops
-- SGE ops
-- SGT ops
-- And ops
-- Or ops
-- Not op
-- If c thn els

-- We specify properties testing certain things, and in subsequent properties, we can only use those
-- constructs which we have already tested. We start by testing the simple things, and build our way up?

-- * Define

data Define = Define Ctx Symbol (SExp Type)

instance Arbitrary Define where
  arbitrary :: Gen Define
  arbitrary = do
    ctx <- genCtx

    t <- arbitrary
    size <- chooseInt (1,5)
    (ctx', e) <- genExp ctx size t

    let (ctx'', name) = fresh ctx' "env"

    return $ Property.Define ctx'' name e

  shrink :: Define -> [Define]
  shrink (Property.Define ctx s e) =
    [ Property.Define (ctx' { toplevel = reverse (toplevel ctx') }) s e
    | ctx' <- shrinkCtx (free e) (ctx { toplevel = reverse (toplevel ctx)}) ] <>
    [ Property.Define ctx s e' | e' <- shrinkSExp e]

instance Show Define where
 show :: Define -> String
 show (Property.Define ctx s body) =
   "===== Env =====\n" <>
     prettyCtx ctx <>
   "\n===== Local defs =====\n" <>
     prettyExp (Syntax.Define s body)

prop_define :: Define -> Property
prop_define (Property.Define ctx s e) = monadicIO $ do
  env <- run $ toEnv ctx
  res1 <- run $ compileAndRun env (Syntax.Define s e)
  guardAgainstError res1

  ires <- run $ compileAndRun env e
  guardAgainstError ires

  let Right (result2, env2) = ires
      res2 = Right (result2, M.insert s result2 env2)

  assert $ res1 == res2

-- * Lambda

data Lambda = Lambda Ctx [Symbol] (SExp Type)

instance Arbitrary Lambda where
  arbitrary :: Gen Lambda
  arbitrary = do
    ctx <- genCtx

    numparams <- chooseInt (1,10)
    ts <- sequence $ replicate numparams arbitrary
    t <- arbitrary

    let (ctx', names) = freshN ctx "p" numparams

    size <- chooseInt (1,5)
    (ctx'', e) <- withLocals ctx' names ts $ \ctx -> genExp ctx size t

    return $ Property.Lambda ctx'' names e

  shrink :: Lambda -> [Lambda]
  shrink (Property.Lambda ctx params e) =
    [ Property.Lambda (ctx' { toplevel = reverse (toplevel ctx') }) params e
    | ctx' <- shrinkCtx (free e \\ params) (ctx { toplevel = reverse (toplevel ctx)}) ] <>
    [ Property.Lambda ctx params e' | e' <- shrinkSExp e]

instance Show Lambda where
 show :: Lambda -> String
 show (Property.Lambda ctx params body) =
  let paramexps = listToCons $ map (Symbol TSymbol Nothing) params in
   "===== Env =====\n" <>
     prettyCtx ctx <>
   "\n===== The Lambda =====\n" <>
     prettyExp (Syntax.Lambda paramexps body)

prop_lambda :: Lambda -> Property
prop_lambda (Property.Lambda ctx params body) = monadicIO $ do
  env <- run $ toEnv ctx
  let paramexps = listToCons $ map (Symbol TSymbol Nothing) params
      nil = Symbol TSymbol Nothing (Sym "nil")
      -- if we had access to the local environment, we would have populated the closure with that here, instead of just using nil
      -- it would also make the property more interesting if we can generate a body using parameters from a local env
      expected@(Right (exp1, env')) = Right (Syntax.Closure paramexps body nil, env)

  r <- run $ compileAndRun env (Syntax.Lambda paramexps body)
  guardAgainstError r
  let Right (exp2, env'') = r

  assert $ r == expected

-- * Closure

data Closure = Closure Ctx [Symbol] (SExp Type) [(Symbol, SExp Type)]

instance Arbitrary Closure where
  arbitrary :: Gen Closure
  arbitrary = do
    ctx <- genCtx

    -- bound vars
    numparams <- chooseInt (1,10)
    ts <- sequence $ replicate numparams arbitrary
    let (ctx', names) = freshN ctx "p" numparams

    -- local environment
    numlocals <- chooseInt (1,10)
    ts2 <- sequence $ replicate numlocals arbitrary
    let (ctx'', localnames) = freshN ctx' "local-var" numlocals
    (ctx''', es) <- genExps ctx'' numlocals ts2

    -- lambda body
    t <- arbitrary
    size <- chooseInt (1,5)
    (ctx'''', e) <- withLocals ctx''' localnames ts2 $ \ctx ->
                      withLocals ctx names ts $ \ctx ->
                        genExp ctx size t

    return $ Property.Closure ctx'''' names e (zip localnames es)

  shrink :: Closure -> [Closure]
  shrink (Property.Closure ctx params e localenv) =
    [ Property.Closure (ctx' { toplevel = reverse (toplevel ctx') }) params e localenv
    | ctx' <- shrinkCtx (free e \\ (params \\ map fst localenv)) (ctx { toplevel = reverse (toplevel ctx)}) ] <>
    [ Property.Closure ctx params e localenv' | localenv' <- applyElementWise (\(s,e) -> [ (s,e') | e' <- shrinkSExp e]) localenv] <>
    [ Property.Closure ctx params e' localenv | e' <- shrinkSExp e]

instance Show Closure where
 show :: Closure -> String
 show (Property.Closure ctx params body localenv) =
  let paramexps = listToCons $ map (Symbol TSymbol Nothing) params
      localenv' = listToCons $ map (\(s,e) -> listToCons [Symbol TSymbol Nothing s,e]) localenv in
   "===== Env =====\n" <>
     prettyCtx ctx <>
   "\n===== The Closure =====\n" <>
     prettyExp (Syntax.Closure paramexps body localenv')

prop_closure :: Closure -> Property
prop_closure (Property.Closure ctx params e localenv) = monadicIO $ do
  env <- run $ toEnv ctx
  let paramexps = listToCons $ map (Symbol TSymbol Nothing) params
      localenvexp = listToCons $ map (\(s,e) -> listToCons [Symbol TSymbol Nothing s, e]) localenv
      closureexp = Syntax.Closure paramexps e localenvexp
  r <- run $ compileAndRun env closureexp
  guardAgainstError r

  assert $ r == Right (closureexp, env) -- evaluating a closure has no effect

-- * Progn

prop_progn :: ArbitraryExpressions -> Property
prop_progn (ArbitraryExpressions ctx es) = monadicIO $ do
  env <- run $ toEnv ctx
  r1 <- foldOverResults env es Nothing (\_ e2 -> e2)
  r2 <- run $ compileAndRun env (Progn (listToCons es))
  guardAgainstError r2

  let Right (res, env')   = r1
  let Right (res', env'') = r2

  assert $ res == res' && env' == env''

-- * Var

-- not general enough,but a good start. This only specifices the behavior of var when there is
-- a single var, followed by the variable. We need further properties that state the behavior in
-- other contexts as well (where more crap is going on in the progn). If we withdraw these
-- properties and turn them into rewrite rules, this property is not enough to rewrite all
-- vars
prop_var :: ArbitraryExpressions -> Property
prop_var (ArbitraryExpressions ctx (e:_)) = monadicIO $ do
  env <- run $ toEnv ctx
  let a = Symbol TSymbol Nothing (Sym "a")
      var = Symbol TSymbol Nothing (Sym "var")
      progn = Symbol TSymbol Nothing (Sym "progn")
      varexp = listToCons [var, a, e]
      wholeexp = listToCons [progn, varexp, a]


  r1 <- run $ compileAndRun env e
  guardAgainstError r1
  r2 <- run $ compileAndRun env wholeexp
  guardAgainstError r2

  assert $ r1 == r2

-- * Let

data Let = Let Ctx [(Symbol, SExp Type)] (SExp Type)

instance Arbitrary Let where
  arbitrary :: Gen Let
  arbitrary = do
    ctx <- genCtx

    numbinds <- chooseInt (1,1)
    ts <- sequence $ replicate numbinds arbitrary
    size <- chooseInt (1,5)
    (ctx', es) <- genExps ctx size ts
    let (ctx'', names) = freshN ctx "local-var" numbinds

    t <- arbitrary
    (ctx''', body) <- withLocals ctx'' names ts $ (\c -> genExp c size t)

    return $ Property.Let ctx (zip names es) body

  shrink :: Let -> [Let]
  shrink (Property.Let ctx binds body) =
    [ Property.Let (ctx' { toplevel = reverse (toplevel ctx') }) binds body
    | ctx' <- shrinkCtx (nub $ free body <> concatMap (free . snd) binds) (ctx { toplevel = reverse (toplevel ctx) })] <>
    [ Property.Let ctx binds' body | binds' <- applyElementWise (\(s,e) -> [ (s,e') | e' <- shrinkSExp e]) binds ] <>
    [ Property.Let ctx binds body' | body' <- shrinkSExp body ]

instance Show Let where
  show (Property.Let ctx binds body) =
    "===== Env =====\n" <>
      prettyCtx ctx <>
    "\n===== Local defs =====\n" <>
      unlines (map (\(s,e) -> "let " <> prettySymbol s <> " = " <> prettyExp e) binds) <>
    "===== In =====\n" <>
      prettyExp body

prop_let :: Let -> Property
prop_let (Property.Let ctx binds body) = monadicIO $ do
  env <- run $ toEnv ctx
  let var = Symbol TSymbol Nothing (Sym "var")
      progn = Symbol TSymbol Nothing (Sym "progn")
      varbinds = listToCons $ map (\(s,e) -> listToCons [Symbol TSymbol Nothing s, e]) binds
      varexps = map (\(s,e) -> listToCons [var, Symbol TSymbol Nothing s, e]) binds
      letexp = Syntax.Let varbinds body
      prognexp = Progn $ listToCons $ varexps <> [body]

  r1 <- run $ compileAndRun env letexp
  guardAgainstError r1
  r2 <- run $ compileAndRun env prognexp
  guardAgainstError r2

  assert $ r1 == r2

-- up until this point, we have properties test the (almost) correct behavior of
-- define
-- lambda
-- closure
-- progn
-- var <-- this one has perhaps not ideal behavior specified
-- let
-- and we can thus use them in subsequent properties

-- * Application

data App = App Ctx [(Symbol, Type)] (SExp Type) [SExp Type]

instance Arbitrary App where
  arbitrary :: Gen App
  arbitrary = do
    ctx <- genCtx

    numparams <- chooseInt (1,10)
    ts <- sequence $ replicate numparams arbitrary
    d <- arbitrary
    s <- chooseInt (1,5)
    let funtype = TFun ts d
        (ctx', names) = freshN ctx "p" numparams
    (ctx'', body) <- withLocals ctx' names ts $ \ctx -> genExp ctx s d
    (ctx''', es) <- genExps ctx s ts

    return $ App ctx''' (zip names ts) body es

  shrink :: App -> [App]
  shrink (App ctx params body args) =
    [ App (ctx' { toplevel = reverse (toplevel ctx') }) params body args
    | ctx' <- shrinkCtx (nub $ free body <> concatMap free args) (ctx { toplevel = reverse (toplevel ctx) })] <>
    [ App ctx params body args' | args' <- applyElementWise shrinkSExp args ] <>
    [ App ctx params body' args | body' <- shrinkSExp body ]

instance Show App where
  show (App ctx params body args) =
    "===== Env =====\n" <>
      prettyCtx ctx <>
    "\n===== Body =====\n" <>
      prettyExp body <>
    "===== Args =====\n" <>
      unlines (map prettyExp args)

prop_app :: App -> Property
prop_app (App ctx params body args) = monadicIO $ do
  env <- run $ toEnv ctx
  let var = Symbol TSymbol Nothing (Sym "var")
      appexp = listToCons $ (Syntax.Lambda (listToCons $ map (Symbol TSymbol Nothing . fst) params) body) : args
      letexp = Syntax.Let (listToCons $ map (\((s,_), e) -> listToCons [Symbol TSymbol Nothing s,e]) (zip params args)) body
  r1 <- run $ compileAndRun env appexp
  guardAgainstError r1
  r2 <- run $ compileAndRun env letexp
  guardAgainstError r2

  assert $ r1 == r2
