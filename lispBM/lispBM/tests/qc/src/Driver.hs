module Driver where

import qualified Data.Binary as B
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Char8 as U
import qualified Data.ByteString.Lazy as BS
import Data.Int
import qualified Data.Map as M
import Data.Word
import GHC.Float
-- import Debug.Trace
import Numeric
import Syntax
import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)
import System.Exit
import System.IO.Temp
import System.Process
import Test.QuickCheck.Monadic
import Data.List

-- * Compile and run (or load and interprete)
type RunResult = Either String (SExp Type, Environment)

-- | Compile and run the given program with the given environment, and get either an error, or
-- the result and final environment back.
--
-- NOTE: This function is supposed to be threadsafe, as it generates a unique directory to
-- use as a scratch space. If we trust the unique name generator to not give us collisions, it
-- is thread-safe.
compileAndRun :: Environment -> SExp Type -> IO (RunResult)
compileAndRun env prog =
  withTempDirectory "" "tmp" $ \fp -> do
    let envin = fp <> "/envin.env"
        envout = fp <> "/envout.env"
        result = fp <> "/result.res"
        progin = fp <> "/prg.lisp"
        lbm = "lbm"
        arguments =
          [ "--load_env=" <> envin,
            "--store_env=" <> envout,
            "--store_res=" <> result,
            "-H 100000",
--            "--silent",
            "--terminate",
            "--src=" <> progin
          ]

    writeFile progin (prettyExp prog)
    storeEnv envin env

    (code, outStr, _) <- readCreateProcessWithExitCode (proc lbm arguments) ""
    case code of
      ExitFailure c -> do
        putStrLn outStr
        return $ Left $ exitcode c
      ExitSuccess -> do
        finenv <- loadEnv envout
        b <- doesFileExist result
        if b
          then do finres <- loadRes result
                  return (Right (finres, finenv))
          else return $ Left ("result.res not available for input $ " <> (prettyExp prog))

-- | Errors that can be thrown. I found these in src/symrepr.c of LBM
errors :: [String]
errors =
  [ "no_match",
    "read_error",
    "type_error",
    "eval_error",
    "out_of_memory",
    "fatal-error",
    "out_of_stack",
    "division_by_zero",
    "variable_not_bound",
    "flash_full"
  ]

exitcodes :: M.Map Int String
exitcodes =
  M.fromList
    [ (1, "out_of_memory_while_flattening"),
      (2, "flattening_maximum_depth"),
      (3, "circular_value"),
      (4, "fatal_error_while_flattening"),
      (5, "flat_value_buffer_too_small"),
      (6, "value_cannot_be_flattened"),
      (7, "invalid_key_in_environment"),
      (8, "lbm_event_queue_full"),
      (9, "invalue_value_in_env_file"),
      (10, "out_of_memory_while_processing_env_file"),
      (11, "invalid_value_size_in_env_file"),
      (12, "invalid_key_in_env_file"),
      (13, "zero_length_key_in_env_file"),
      (14, "invalid_env_file"),
      (15, "unable_to_open_env_file"),
      (16, "unable_to_init_lbm"),
      (17, "error_flatten_no_mem"),
      (18, "unable_to_create_symbol"),
      (19, "invalid_source_file"),
      (20, "unable_to_access_symbol_string"),
      (21, "critical_error"),
      (22, "symbol_table_too_big"),
      (23, "env_population_timeout")
    ]

exitcode :: Int -> String
exitcode c = case M.lookup c exitcodes of
  Just str -> str
  Nothing -> concat ["could not look up code: ", show c]

-- | Takes a result and returns it, but falsifies the property if the result is one of the error-symbols
guardAgainstError :: Either String (SExp Type, Environment) -> PropertyM IO (Either String (SExp Type, Environment))
guardAgainstError (Left str) =
  assertWith False ("erroneous exitcode from lbm: " <> str) >> return (Left str)
guardAgainstError r@(Right (Symbol _ _ (Sym s), _)) | s `elem` errors =
  assertWith False ("error was thrown: " <> s) >> return r
guardAgainstError r = return r

isLBMError :: Either String (SExp Type, Environment) -> Bool
isLBMError (Left str) = True
isLBMError _ = False

isLBMException :: Either String (SExp Type, Environment) -> Bool
isLBMException r@(Right (Symbol _ _ (Sym s), _)) = s `elem` errors 
isLBMException _ = False

foldOverResults :: Environment -> [SExp Type] -> Maybe (SExp Type) -> (SExp Type -> SExp Type -> SExp Type) -> PropertyM IO (Either String (SExp Type, Environment))
foldOverResults env [] (Just acc) _ = return $ Right (acc, env)
foldOverResults env (e:es) acc f = do
  r <- run $ compileAndRun env e -- (listToCons $ ( Symbol TSymbol (Sym "eval")) : [e])
  Right (res, env') <- guardAgainstError r
  case acc of
    Just r' -> foldOverResults env' es (Just $ f r' res) f
    Nothing -> foldOverResults env' es (Just res) f

foldUntil :: Environment -> [SExp Type] -> (SExp Type -> Bool) -> PropertyM IO (Either String (SExp Type, Environment))
foldUntil env [e] test = do
  x <- run $ compileAndRun env e
  guardAgainstError x
  return $ x
foldUntil env (e:es) test = do
  x <- run $ compileAndRun env e
  guardAgainstError x
  case x of
    Right (r, _) | test r -> return x
    Right (_, env') -> foldUntil env' es test
    _ -> error $ "error in foldUntil, result was: " <> show x

foldUntilNotEqualLazy :: Environment -> [SExp Type] -> Maybe (SExp Type) -> PropertyM IO (Either String (SExp Type, Environment))
foldUntilNotEqualLazy env [] _ = return $ Right (Symbol TSymbol Nothing (Sym "t"), env)
foldUntilNotEqualLazy env (e:es) prev = do
  x <- run $ compileAndRun env e
  guardAgainstError x
  let Right (s,env') = x
  case prev of
    Nothing -> foldUntilNotEqualLazy env' es (Just s)
    Just s' | s == s' -> foldUntilNotEqualLazy env' es (Just s)
            | otherwise -> return $ Right (Symbol TSymbol Nothing (Sym "nil"), env')

foldUntilNotEqual :: Environment -> [SExp Type] -> [SExp Type] -> PropertyM IO (Either String (SExp Type, Environment))
foldUntilNotEqual env [] ress = if length (nub ress) == 1
  then return $ Right (Symbol TSymbol Nothing (Sym "t"), env)
  else return $ Right (Symbol TSymbol Nothing (Sym "nil"), env)
foldUntilNotEqual env (e:es) ress = do
  x <- run $ compileAndRun env e
  guardAgainstError x
  let Right (s, env') = x
  foldUntilNotEqual env' es (s:ress)

foldUntilNotEqualNumeric :: Environment -> [SExp Type] -> [SExp Type] -> PropertyM IO (Either String (SExp Type, Environment))
foldUntilNotEqualNumeric env [] ress = if length (nub ress) == 1
  then return $ Right (Symbol TSymbol Nothing (Sym "t"), env)
  else return $ Right (Symbol TSymbol Nothing (Sym "nil"), env)
foldUntilNotEqualNumeric env (e:es) ress = do
  x <- run $ compileAndRun env e
  guardAgainstError x
  let Right (s, env') = x
  foldUntilNotEqualNumeric env' es (toDouble s:ress)

toDouble :: SExp Type -> SExp Type
toDouble (ILit _ i) = FLit TDouble (fromInteger i)
toDouble (FLit TFloat f) = FLit TDouble f
toDouble e = e


-- foldUntilNotEqual env (e:es) prev = do
--   x <- run $ compileAndRun env e
--   guardAgainstError x
--   let Right (s,env') = x
--   case prev of
--     Nothing -> foldUntilNotEqual env' es (Just s)
--     Just s' | s == s' -> foldUntilNotEqual env' es (Just s)
--             | otherwise -> return $ Right (Symbol TSymbol Nothing (Sym "nil"), env')

{-

file: 32 bits in start of file, indicates length of file in bytes of result file

programmet

-}

-- # bytes in result

type Environment = M.Map Symbol (SExp Type)

lookup :: Environment -> Symbol -> SExp Type
lookup e s = case M.lookup s e of
  Just e -> e
  Nothing -> error $ "symbol not found: " <> show s

toSExp :: Environment -> SExp Type -> SExp Type
toSExp ctx (Symbol _ _ s) = Driver.lookup ctx s
toSExp _ e = e

data Binding
  = NonExistant
  | Bound Symbol (SExp Type)
  deriving (Show)

-- | Takes two environments and returns pairs of bindings for which they disagreed (where the
-- associated value, if any, was not identical). If this function returns an empty list we can
-- conclude that the environments are identical.
envDiff :: Environment -> Environment -> [(Binding, Binding)]
envDiff e1 e2 =
  let (diff0, e3) = diffIt [] (M.assocs e1) e2
   in if M.null e3
        then diff0
        else concat [(allAlone (M.assocs e3)), diff0]
  where
    -- \| Traverses the name,value-pairs of one environment and inspects whether the names exist in the given
    -- environment. Returns a list of pairs indicating which differing values were associated (if any) with which
    -- names.
    -- Also returns the remainder of the given environment
    diffIt :: [(Binding, Binding)] -> [(Symbol, SExp Type)] -> Environment -> ([(Binding, Binding)], Environment)
    diffIt ack [] e = (ack, e)
    diffIt ack ((sym, val) : rest) e2 =
      case M.lookup sym e2 of
        Just r ->
          if r == val
            then diffIt ack rest (M.delete sym e2)
            else diffIt ((Bound sym val, Bound sym r) : ack) rest (M.delete sym e2)
        Nothing -> diffIt ((Bound sym val, NonExistant) : ack) rest e2

    -- \| Takes a list of name,value-pairs and marks them all as non-existing in the left environment
    allAlone :: [(Symbol, SExp Type)] -> [(Binding, Binding)]
    allAlone [] = []
    allAlone ((sym, val) : xs) = (NonExistant, Bound sym val) : (allAlone xs)

testEnv :: M.Map Symbol (SExp Type)
testEnv =
  M.fromList
    [ (Sym "apa", (ILit TInt28 5)),
      (Sym "apa1", (ILit TInt28 (-10))),
      (Sym "bepa", (ILit TUInt32 100)),
      (Sym "cepa", (Symbol TSymbol Nothing (Sym "kurt-russel"))),
      (Sym "depa", (FLit TFloat 3.14)),
      (Sym "eepa", (FLit TDouble 6.28)),
      (Sym "fepa", listToCons [i28 1, i32 2, u28 3, u32 4]),
      (Sym "gepa", str "Hello World")
    ]

sCons :: Word8
sCons = 0x1

sSym :: Word8
sSym = 0x3

sByte :: Word8
sByte = 0x4

sI :: Word8
sI = 0x5

sU :: Word8
sU = 0x6

sI32 :: Word8
sI32 = 0x7

sU32 :: Word8
sU32 = 0x8

sFloat :: Word8
sFloat = 0x9

sDouble :: Word8
sDouble = 0xC

sI64 :: Word8
sI64 = 0xA

sU64 :: Word8
sU64 = 0xB

sArray :: Word8
sArray = 0xD

c :: Char -> Word8
c = toEnum . fromEnum

w :: Word8 -> Char
w = toEnum . fromEnum

-- Some of these incorrect for negative numbers, surely.
-- Not sure how to do it...

putByte :: (Integral a) => a -> Put
putByte a = putWord8 ((fromIntegral a) :: Word8)

putI28 :: (Integral a) => a -> Put
putI28 a =
  if (a > 2 ^ 27 || a < -(2 ^ 27 - 1))
    then error "Out of range"
    else
      let w = (fromIntegral a :: Word32)
       in if (a < 0)
            then putWord32be (w .|. (0xF0000000 :: Word32))
            else putWord32be w

putU28 :: (Integral a) => a -> Put
putU28 a = putWord32be ((fromIntegral (a `mod` 2 ^ 28)) :: Word32)

putI32 :: (Integral a) => a -> Put
putI32 a = putWord32be ((fromIntegral (a `mod` 2 ^ 32)) :: Word32)

putU32 :: (Integral a) => a -> Put
putU32 a = putWord32be ((fromIntegral (a `mod` 2 ^ 32)) :: Word32)

putI64 :: (Integral a) => a -> Put
putI64 a = putWord64be ((fromIntegral a) :: Word64)

putU64 :: (Integral a) => a -> Put
putU64 a = putWord64be ((fromIntegral a) :: Word64)

putFloat :: Float -> Put
putFloat a = putWord32be (castFloatToWord32 a)

putDouble :: Double -> Put
putDouble a = putWord64be (castDoubleToWord64 a)

emptyBS = BS.empty

lenW32 :: (Integral a) => a -> BS.ByteString
lenW32 a = B.encode ((fromIntegral a) :: Word32)

toW32 :: (Integral a) => a -> Word32
toW32 a = fromIntegral a

-- Todo: Uncertain about the terminating 0 for C strings..
encSExp :: SExp Type -> Put
encSExp (Symbol _ _ (Sym str)) =
  do
    putWord8 sSym
    putByteString (U.pack str)
    putWord8 0
encSExp (Cons _ a b) =
  do
    putWord8 sCons
    encSExp a
    encSExp b
encSExp (Quote _ a) =
  let t = getf a
      p1 = Cons (TPair t TSymbol) a (Symbol TSymbol Nothing (Sym "nil"))
      p2 = Cons (TPair TSymbol (TPair t TSymbol)) (Symbol TSymbol Nothing (Sym "quote")) p1
   in encSExp p2
encSExp (Array _ s) =
  do
    putWord8 sArray
    putWord32be (fromIntegral (BS.length s))
    putByteString (BS.toStrict s)
encSExp (ILit t i)
  | t == TByte = do putWord8 sByte; putByte i
  | t == TInt28 = do putWord8 sI; putI28 i
  | t == TUInt28 = do putWord8 sU; putU28 i
  | t == TInt32 = do putWord8 sI32; putI32 i
  | t == TUInt32 = do putWord8 sU32; putU32 i
  | t == TInt64 = do putWord8 sI64; putI64 i
  | t == TUInt64 = do putWord8 sU64; putU64 i
  | otherwise = error "Im not done yet"
encSExp (FLit t v)
  | t == TFloat = do putWord8 sFloat; putFloat (realToFrac v)
  | t == TDouble = do putWord8 sDouble; putDouble v
  | otherwise = error "Im not done yet"

encName :: String -> BS.ByteString
encName name =
  runPut $ do
    putWord32le (toW32 (length name))
    putByteString (U.pack name)

encEnv :: Environment -> BS.ByteString
encEnv e = BS.concat (map f (M.assocs e))
  where
    f (Sym k, v) =
      let bs1 = encName k
          bs2 = runPut (encSExp v)
          size = BS.length bs2
          bs = runPut $ putWord32le (toW32 size)
       in BS.concat [bs1, bs, bs2]

storeTestEnv :: IO ()
storeTestEnv = BS.writeFile "testrobert.env" $ encEnv testEnv

-- Getter of environments

getNullTerminatedString :: Get String
getNullTerminatedString =
  do
    bstr <- g []
    return $ reverse $ map w bstr
  where
    g :: [Word8] -> Get [Word8]
    g ack =
      do
        b <- getWord8
        if (b == 0)
          then return (ack) -- Read the 0 but dont add it to the IR
          else g (b : ack)

processQuote :: SExp Type -> SExp Type
processQuote (Cons (TPair TSymbol (TPair _ TSymbol)) (Symbol TSymbol Nothing (Sym "quote")) (Cons (TPair t TSymbol) a (Symbol TSymbol Nothing (Sym "nil")))) = (Quote TSymbol a)
processQuote a = a

getSExp :: Get (SExp Type)
getSExp =
  do
    tag <- getWord8
    --    traceM $ "tag: " ++ (show tag)
    case tag of
      0x1 {- sCons -} ->
        do
          a1 <- getSExp
          a2 <- getSExp
          return $ processQuote $ Cons (TPair (getf a1) (getf a2)) a1 a2
      0x3 {- sSym -} ->
        do
          str <- getNullTerminatedString
          if (str == "nil")
            then return (Symbol TSymbol Nothing (Sym "nil"))
            else return (Symbol TSymbol Nothing (Sym str))
      0x4 {- sByte -} ->
        do
          b <- getWord8
          return (ILit TByte (fromIntegral b))
      0x5 {- sI -} ->
        do
          w <- getWord32be
          let i =
                if (testBit w 27)
                  then -(complement ((w .|. (0xF0000000 :: Word32)))) - 1
                  else w

          return (ILit TInt28 (fromIntegral ((fromIntegral i) :: Int32)))
      0x6 {- sU -} ->
        do
          w <- getWord32be
          return (ILit TUInt28 (fromIntegral w))
      0x7 {- sI32 -} ->
        do
          w <- getWord32be
          return (ILit TInt32 (fromIntegral ((fromIntegral w) :: Int32)))
      0x8 {- sU32 -} ->
        do
          w <- getWord32be
          return (ILit TUInt32 (fromIntegral w))
      0xA {- sI64 -} ->
        do
          w <- getWord64be
          return (ILit TInt64 (fromIntegral ((fromIntegral w) :: Int64)))
      0xB {- sU64 -} ->
        do
          w <- getWord64be
          return (ILit TUInt64 (fromIntegral w))
      0x9 {- sFloat -} ->
        do
          w <- getWord32be
          return (FLit TFloat (realToFrac (castWord32ToFloat w)))
      0xC {- sDouble -} ->
        do
          w <- getWord64be
          return (FLit TDouble (castWord64ToDouble w))
      0xD {- sArray -} ->
        do
          size <- getWord32be
          dat <- getByteString (fromIntegral size)
          return (Array TArray (BS.fromStrict dat))
      _ -> error $ "Invalid " ++ show tag

-- TODO: Should probably be some error handling here.
getBinding :: Get (String, SExp Type)
getBinding =
  do
    nameSize <- getWord32le
    name <- getByteString (fromIntegral nameSize)
    _ <- getWord32le -- Throw away value size
    sexp <- getSExp
    return (U.unpack name, sexp)

getEnv :: Get Environment
getEnv = getEnvE M.empty
  where
    getEnvE :: Environment -> Get Environment
    getEnvE e =
      do
        b <- isEmpty
        if (b)
          then return e
          else do
            (name, exp) <- getBinding
            getEnvE (M.insert (Sym name) exp e)

decEnv :: BS.ByteString -> Environment
decEnv bs = runGet getEnv bs

decSExp :: BS.ByteString -> SExp Type
decSExp bs = runGet getSExp bs

-- ******************** Loading and storing ********************

storeEnv :: FilePath -> Environment -> IO ()
storeEnv fp env = do
  let e = encEnv env
  BS.writeFile fp e

loadEnv :: FilePath -> IO Environment
loadEnv fp = do
  bs <- BS.readFile fp
  return $ decEnv bs

loadRes :: FilePath -> IO (SExp Type)
loadRes fp = do
  bs <- BS.readFile fp
  return $
    runGet
      ( do
          _ <- getWord32le -- Throw away size
          sexp <- getSExp
          return sexp
      )
      bs

{-@

main :: IO ()
main = do
    vi <- newIORef 0
    quickCheck $ test_prop vi

-- withTmpDIrectory....

test_prop :: IORef Int -> Ctx -> Property
test_prop vi ctx = monadicIO $ do
    i <- run $ atomicModifyIORef vi \i -> (i, i+1)
    compileAndRun i (show ctx)

prop_env :: Environment -> Property
prop_env env = decode (encode env) == env

@-}

{-

(e1,s) `car` (nil,s')

(and e1 _, s) `car` (nil, s')

==

(and e1 e2 e3, s) `eval` (nil, s')

(e1,s) `car` (_,s')
(and e2 e3, s') `car` (r, s'')

==

(and e1 e2 e3, s) `car` (r, s'')

-}
