module Oracles where

import Syntax hiding (u28, u32, u64, i28, i32, i64, f32, f64)
import Driver

import Data.Int
import Data.Word
import Data.Bits
import Unsafe.Coerce

import Data.Fixed

{- In this module we place our oracles. We wish to define inductive properties, but we can not test _everything_
inductively. We are, however, making the problem significantly smaller. Rather than requiring an oracle for
an entire language, it is enough that we are provided with an oracle for e.g. numeric operations. -}

-- * Conversions

u8 :: Integer -> Word8
u8 = fromInteger

u32 :: Integer -> Word32
u32 = fromInteger

i32 :: Integer -> Int32
i32 = fromInteger

u64 :: Integer -> Word64
u64 = fromInteger

i64 :: Integer -> Int64
i64 = fromInteger

f32 :: Double -> Float
f32 = realToFrac

f64 :: Float -> Double
f64 = realToFrac

-- * Addition

addExps :: SExp Type -> SExp Type -> SExp Type
addExps (ILit t1 i1) (ILit t2 i2) = addIntegerType (max t1 t2) i1 i2
addExps (ILit t1 i1) (FLit t2 d2) = addFloatingType (max t1 t2) (fromInteger i1) d2
addExps (FLit t1 d1) (ILit t2 i2) = addFloatingType (max t1 t2) d1 (fromInteger i2)
addExps (FLit t1 d1) (FLit t2 d2) = addFloatingType (max t1 t2) d1 d2
addExps e1 e2 = error $ "can not add these two expressions of type " <> show (getf e1) <> " and " <> show (getf e2)

-- this remains unused, why?
fromI28 :: Integral a => a -> Int32
fromI28 a = unsafeCoerce a
  where a' = fromIntegral a :: Word32

i28Add :: Int32 -> Int32 -> Int32
i28Add a b = 
  let a' = shift (unsafeCoerce a :: Word32) 4
      b' = shift (unsafeCoerce b :: Word32) 4
      s = shift (a' + b') (-4)
  in if (testBit s 27)
     then - (complement (unsafeCoerce (s .|. 0xF0000000) :: Int32)  + 1)
     else unsafeCoerce s

addIntegerType :: Type -> Integer -> Integer -> SExp Type
addIntegerType t a b = case t of
  TByte -> ILit TByte (toInteger $ u8 a + u8 b)
  TUInt28 -> let a' = (fromIntegral a) :: Word32
                 b' = (fromIntegral b) :: Word32
             in  ILit TUInt28 $ toInteger ((a' + b') `mod` 2^28)
  TInt28 ->  ILit TInt28 (toInteger (i28Add (fromIntegral a) (fromIntegral b)))
  TUInt32 -> ILit TUInt32 (toInteger $ u32 a + u32 b)
  TInt32 -> ILit TInt32 (toInteger $ i32 a + i32 b)
  TUInt64 -> ILit TUInt64 (toInteger $ u64 a + u64 b)
  TInt64 -> ILit TInt64 (toInteger $ i64 a + i64 b)
  _ -> error $ "expected integer type, but instead got " <> show t

addFloatingType :: Type -> Double -> Double -> SExp Type
addFloatingType t d1 d2 = case t of
  TFloat -> FLit TFloat (f64 $ f32 d1 + f32 d2)
  TDouble -> FLit TDouble (d1 + d2)
  _ -> error $ "expected floating type, but instead got " <> show t

-- * Subtraction

subExps :: SExp Type -> SExp Type -> SExp Type
subExps (ILit t1 i1) (ILit t2 i2) = subIntegerType (max t1 t2) i1 i2
subExps (ILit t1 i1) (FLit t2 d2) = subFloatingType (max t1 t2) (fromInteger i1) d2
subExps (FLit t1 d1) (ILit t2 i2) = subFloatingType (max t1 t2) d1 (fromInteger i2)
subExps (FLit t1 d1) (FLit t2 d2) = subFloatingType (max t1 t2) d1 d2
subExps e1 e2 = error $ "can not subtract these two expressions of type " <> show (getf e1) <> " and " <> show (getf e2)

i28Sub :: Int32 -> Int32 -> Int32
i28Sub a b = 
  let a' = shift (unsafeCoerce a :: Word32) 4
      b' = shift (unsafeCoerce b :: Word32) 4
      s = shift (a' - b') (-4)
  in if (testBit s 27)
     then - (complement (unsafeCoerce (s .|. 0xF0000000) :: Int32)  + 1)
     else unsafeCoerce s

subIntegerType :: Type -> Integer -> Integer -> SExp Type
subIntegerType t a b = case t of
  TByte -> ILit TByte (toInteger $ u8 a - u8 b)
  TUInt28 -> let a' = (fromIntegral a) :: Word32
                 b' = (fromIntegral b) :: Word32
             in  ILit TUInt28 $ toInteger ((a' - b') `mod` 2^28)
  TInt28 ->  ILit TInt28 (toInteger (i28Sub (fromIntegral a) (fromIntegral b)))
  TUInt32 -> ILit TUInt32 (toInteger $ u32 a - u32 b)
  TInt32 -> ILit TInt32 (toInteger $ i32 a - i32 b)
  TUInt64 -> ILit TUInt64 (toInteger $ u64 a - u64 b)
  TInt64 -> ILit TInt64 (toInteger $ i64 a - i64 b)
  _ -> error $ "expected integer type, but instead got " <> show t

subFloatingType :: Type -> Double -> Double -> SExp Type
subFloatingType t d1 d2 = case t of
  TFloat -> FLit TFloat (f64 $ f32 d1 - f32 d2)
  TDouble -> FLit TDouble (d1 - d2)
  _ -> error $ "expected floating type, but instead got " <> show t

-- * Negation

negateExp :: SExp Type -> SExp Type
negateExp (ILit t i) = case t of
    TByte -> ILit t (toInteger $ negate $ u8 i)
    TUInt28 -> let i' = shift (unsafeCoerce i :: Word32) 4
               in ILit t $ toInteger $ shift (negate i') (-4)
    TInt28 -> ILit t (toInteger (i28Negate (fromIntegral i)))
    TUInt32 -> ILit t (toInteger $ negate $ u32 i)
    TInt32 -> ILit t (toInteger $ negate $ i32 i)
    TUInt64 -> ILit t (toInteger $ negate $ u64 i)
    TInt64 -> ILit t (toInteger $ negate $ i64 i)
negateExp (FLit t f) = case t of
    TFloat -> FLit t (f64 $ negate $ f32 f)
    TDouble -> FLit t (negate f)
negateExp e = e

i28Negate :: Int32 -> Int32
i28Negate i =
    let i' = shift (unsafeCoerce i :: Word32) 4
        s = shift (negate i') (-4)
    in if testBit s 27
        then - (complement (unsafeCoerce (s .|. 0xF0000000) :: Int32) + 1)
        else unsafeCoerce s

-- * Modulus

modExps :: SExp Type -> SExp Type -> SExp Type
modExps (ILit t1 i1) (ILit t2 i2) = modIntegerType (max t1 t2) i1 i2
modExps (ILit t1 i1) (FLit t2 d2) = modFloatingType (max t1 t2) (fromInteger i1) d2
modExps (FLit t1 d1) (ILit t2 i2) = modFloatingType (max t1 t2) d1 (fromInteger i2)
modExps (FLit t1 d1) (FLit t2 d2) = modFloatingType (max t1 t2) d1 d2
modExps e1 e2 = error $ "can not mod these two expressions of type " <> show (getf e1) <> " and " <> show (getf e2)

i28Mod :: Int32 -> Int32 -> Int32
i28Mod a b = 
  let a' = shift (unsafeCoerce a :: Word32) 4
      b' = shift (unsafeCoerce b :: Word32) 4
      s = shift (a' `mod` b') (-4)
  in if (testBit s 27)
     then - (complement (unsafeCoerce (s .|. 0xF0000000) :: Int32)  + 1)
     else unsafeCoerce s

modIntegerType :: Type -> Integer -> Integer -> SExp Type
modIntegerType t a b = case t of
  TByte -> ILit TByte (toInteger $ u8 a `mod` u8 b)
  TUInt28 -> let a' = (fromIntegral a) :: Word32
                 b' = (fromIntegral b) :: Word32
             in  ILit TUInt28 $ toInteger ((a' `mod` b') `mod` 2^28)
  TInt28 ->  ILit TInt28 (toInteger (i28Mod (fromIntegral a) (fromIntegral b)))
  TUInt32 -> ILit TUInt32 (toInteger $ u32 a `mod` u32 b)
  TInt32 -> ILit TInt32 (toInteger $ i32 a `mod` i32 b)
  TUInt64 -> ILit TUInt64 (toInteger $ u64 a `mod` u64 b)
  TInt64 -> ILit TInt64 (toInteger $ i64 a `mod` i64 b)
  _ -> error $ "expected integer type, but instead got " <> show t

modFloatingType :: Type -> Double -> Double -> SExp Type
modFloatingType t d1 d2 = case t of
  TFloat -> FLit TFloat (f64 $ f32 d1 `mod'` f32 d2)
  TDouble -> FLit TDouble (d1 `mod'` d2)
  _ -> error $ "expected floating type, but instead got " <> show t

-- * Is Zero

isZero :: SExp Type -> Bool
isZero (ILit _ 0) = True
isZero (FLit _ 0.0) = True
isZero _ = False