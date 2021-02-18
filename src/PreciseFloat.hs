module PreciseFloat where

import Lib
import Vector as V

data Bit = BZero | BOne deriving (Eq, Ord)
data Sign = Plus | Minus deriving (Show, Eq)

instance Show Bit where
  show BZero = "0"
  show BOne = "1"

type Bits n = Vector n Bit
type Sixteen = Mul Two Eight
type ThirtyTwo = Mul Two Sixteen
type SixtyFour = Mul Two ThirtyTwo
type OneTwoEight = Mul Two SixtyFour

genericBinOp :: (Bit -> Bit -> Bool) -> Bits n -> Bits n -> Bits n
genericBinOp f b0s b1s = Lib.zipWith applyF b0s b1s
 where
  applyF b0 b1 | f b0 b1   = BOne
               | otherwise = BZero

zipWithDefault :: (a -> b -> c) -> [a] -> [b] -> a -> b -> [c]
zipWithDefault _ []     []     _  _  = []
zipWithDefault f as@[]  (b:bs) a' b' = f a' b : zipWithDefault f as bs a' b'
zipWithDefault f (a:as) bs@[]  a' b' = f a b' : zipWithDefault f as bs a' b'
zipWithDefault f (a:as) (b:bs) a' b' = f a b : zipWithDefault f as bs a' b'

getBits :: (KnownNat n) => [Bit] -> Bits n
getBits = vecFromListWithDefault BZero

bitShiftRight'' :: Bits ( 'Succ n) -> Bits n
bitShiftRight'' (b:+(VecSing b')) | b' == BOne = VecSing BOne
                                  | otherwise  = VecSing b
bitShiftRight'' (b:+bs@(_:+_)) = b :+ bitShiftRight'' bs

bitShiftRight' :: Bit -> Bits n -> Bits n
bitShiftRight' b (   VecSing _) = VecSing b
bitShiftRight' b bs@(_:+_     ) = b :+ (bitShiftRight'' bs)

bitShiftRight :: (Integral a) => a -> Bit -> Bits n -> Bits n
bitShiftRight i b bs | i <= 0    = bs
                     | otherwise = bitShiftRight (i - 1) b (bitShiftRight' b bs)

bitShiftLeft' :: Bit -> Bits n -> Bits n
bitShiftLeft' b (VecSing _) = VecSing b
bitShiftLeft' b (_:+bs    ) = appendVal b bs

bitShiftLeft :: (Integral a) => a -> Bit -> Bits n -> Bits n
bitShiftLeft i b bs | i <= 0    = bs
                    | otherwise = bitShiftLeft (i - 1) b (bitShiftLeft' b bs)

fromBit :: Num a => Bit -> a
fromBit BZero = 0
fromBit BOne  = 1

toBit :: Integral a => a -> Bit
toBit 0 = BZero
toBit 1 = BOne
toBit x = toBit $ mod x 2

showBits :: Bits n -> String
showBits bs = foldr (:) "" (fmap (head . show) bs)

fromSign :: Num a => Sign -> (a -> a)
fromSign Plus  = id
fromSign Minus = negate

getSign :: (Num a, Eq a) => a -> Sign
getSign x | signum x == (-1) = Minus
          | otherwise        = Plus

-- add two bits, and a carry bit, and return a bit and the carry bit
addBits :: Bit -> Bit -> Bit -> (Bit, Bit)
addBits BZero BZero b     = (b, BZero)
addBits BZero b     BZero = (b, BZero)
addBits b     BZero BZero = (b, BZero)
addBits BOne  BOne  BOne  = (BOne, BOne)
addBits _     _     _     = (BZero, BOne)

-- add together a sequence of bits in order, carrying a carry bit
addBinary' :: Bits n -> Bits n -> Bit -> Bits ( 'Succ n)
addBinary' (VecSing b0) (VecSing b1) bc = b :+ VecSing bc'
  where (b, bc') = addBits b0 b1 bc
addBinary' (b0:+b0s) (b1:+b1s) bc = b :+ addBinary' b0s b1s bc'
  where (b, bc') = addBits b0 b1 bc

addBinary :: Bits n -> Bits n -> Bits ( 'Succ n)
addBinary b0s b1s =
  V.reverse $ addBinary' (V.reverse b0s) (V.reverse b1s) BZero

-- Sign is the sign of the number, Integer is the exponent, Binary is the mantissa
data PreciseFloat n = PF Sign Integer (Bits n)

getBitsPF :: PreciseFloat n -> Bits n
getBitsPF (PF _ _ bs) = bs
getExponentPF :: PreciseFloat n -> Integer
getExponentPF (PF _ e _) = e
getSignPF :: PreciseFloat n -> Sign
getSignPF (PF s _ _) = s

toFloating :: Floating a => PreciseFloat n -> a
toFloating (PF s e (VecSing b)) =
  (fromSign s) (fromBit b) * (2 ** (fromInteger e))
toFloating (PF s e (b:+bs)) =
  (fromSign s) (fromBit b) * (2 ** (fromInteger e)) + toFloating
    (PF s (e - 1) bs)

showAsDouble :: PreciseFloat n -> String
showAsDouble pf = show $ ((toFloating pf) :: Double)

instance Show (PreciseFloat n) where
  show (PF s e bs) = "PF " ++ show s ++ " " ++ show e ++ " " ++ showBits bs

toBinaryIntegral' :: Integral a => a -> [Bit]
toBinaryIntegral' 0 = []
toBinaryIntegral' x = toBinaryIntegral' (quot x 2) ++ [toBit x]

toBinaryIntegral :: Integral a => a -> (Sign, [Bit])
toBinaryIntegral x = (getSign x, toBinaryIntegral' $ abs x)

toBinaryFractional' :: (RealFrac a) => a -> [Bit]
toBinaryFractional' 0 = []
toBinaryFractional' x | intPart > 0 = BOne : toBinaryFractional' fracPart
                      | otherwise   = BZero : toBinaryFractional' fracPart
  where (intPart :: Integer, fracPart) = properFraction (x * 2)

toBinaryFractional :: (RealFrac a) => a -> (Sign, ([Bit], [Bit]))
toBinaryFractional x =
  (getSign x, (toBinaryIntegral' intPart, toBinaryFractional' fracPart))
 where
  x'                             = abs x
  (intPart :: Integer, fracPart) = properFraction x'

toPreciseFloatIntegral :: (Integral a, KnownNat n) => a -> PreciseFloat n
toPreciseFloatIntegral x = PF s
                              (fromIntegral (length intPart) - 1)
                              (getBits intPart)
  where (s, intPart) = toBinaryIntegral x

toPreciseFloat :: (RealFrac a, KnownNat n) => a -> PreciseFloat n
toPreciseFloat x
  | length intPart > 0 = PF s
                            (fromIntegral (length intPart) - 1)
                            (getBits $ intPart ++ fracPart)
  | otherwise = PF s
                   ((-fromIntegral fracPartZeroes) - 1)
                   (getBits $ drop fracPartZeroes fracPart)
 where
  (s, (intPart, fracPart)) = toBinaryFractional x
  fracPartZeroes           = length $ takeWhile (== BZero) fracPart

-- https://cs.stackexchange.com/questions/91510/how-to-add-ieee-754-floating-point-numbers
addPF :: PreciseFloat n -> PreciseFloat n -> PreciseFloat n
addPF (PF s1 e1 bs1) (PF s2 e2 bs2) = undefined
 where
  appendZeroes bs = appendVal BZero $ appendVal BZero $ appendVal BZero bs
  maxExp         = max e1 e2
  bs1'           = bitShiftRight (maxExp - e1) BZero $ appendZeroes bs1
  bs2'           = bitShiftRight (maxExp - e2) BZero $ appendZeroes bs2
  sum | s1 == s2 = addBinary bs1' bs2'


-- sameExp :: PreciseFloat -> PreciseFloat -> (PreciseFloat, PreciseFloat)
-- sameExp pf@(PF s e bs) pf'@(PF s' e' bs')
--   | e < e' = (PF s e' ((replicate (fromInteger (e' - e)) BZero) bs), pf')
--   | e > e' = (pf, PF s' e ((replicate (fromInteger (e - e')) BZero)++bs'))
--   | otherwise = (pf, pf')

-- addPFs :: PreciseFloat -> PreciseFloat -> PreciseFloat
-- addPFs (PF _ _ []) pf = pf
-- addPFs pf (PF _ _ []) = pf
-- addPFs pf1 pf2
--   | s == s' = PF s newe binsum
--   where (pf1'@(PF s e bs), pf2'@(PF s' e' bs')) = sameExp pf1 pf2
--         binSum = addBinary bs bs'
--         maxlen = max (length bs) (length bs')
--         newe | length binSum > maxlen = e + 1
--              | otherwise = e

-- instance Num PreciseFloat where
--   fromInteger = toPreciseFloatIntegral
--   negate (PF Plus e bs) = PF Minus e bs
--   negate (PF Minus e bs) = PF Plus e bs
--   abs (PF _ e bs) = PF Plus e bs
--   -- signum (PF _ _ []) = 0
--   -- signum (PF Plus _ _) = 1
--   signum (PF Minus _ _) = -1
--   -- (+) (PF _ _ []) pf' = pf'
--   -- (+) pf (PF _ _ []) = pf



