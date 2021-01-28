module PreciseFloat where

import Lib
import qualified Data.Foldable as F (toList)

data Bit = BZero | BOne deriving (Show, Eq, Ord)
-- type Binary = [Bit]
data Sign = Plus | Minus deriving (Show, Eq)

data Stream a = Stream a (Stream a)

type BitStream = Stream Bit

bZeroes :: BitStream
bZeroes = Stream BZero bZeroes

toList :: Stream a -> [a]
toList (Stream b bs) = b : toList bs

fromListWithTail :: [a] -> Stream a -> Stream a
fromListWithTail as ss = foldr Stream ss as

fromListWithDefault :: a -> [a] -> Stream a
fromListWithDefault a as = fromListWithTail as z
  where z = Stream a z

fromListBits :: [Bit] -> BitStream
fromListBits = fromListWithDefault BZero

take' :: Integral a => a -> [b] -> [b]
take' _ [] = []
take' i (b:bs)
  | i <= 0 = []
  | otherwise = b : take' (i-1) bs

takeS :: Integral a => a -> Stream b -> [b]
takeS i s = take' i (toList s)

data Streams = BS BitStream | AS AddStream

class BinOpStream a where
  createStream :: BitStream -> BitStream -> a
  evaluateTo :: Integral b => b -> a -> BitStream

data AddStream = AddStream BitStream BitStream

instance BinOpStream AddStream where
  createStream = AddStream
  evaluateTo i (AddStream as bs) = fromListBits $ F.toList $ addBinary as' bs'
    where as' = takeS i as
          bs' = takeS i bs

defaultEvalLen :: Integer
defaultEvalLen = 64

instance Show Streams where
  show (BS bs) = "Base " ++ show (takeS defaultEvalLen bs)
  show (AS (AddStream as bs)) = "AS " ++ show (takeS defaultEvalLen as) ++ " " ++ show (takeS defaultEvalLen bs)

fromBit :: Num a => Bit -> a
fromBit BZero = 0
fromBit BOne = 1

toBit :: Integral a => a -> Bit
toBit 0 = BZero
toBit 1 = BOne
toBit x = toBit $ mod x 2

fromSign :: Num a => Sign -> (a -> a)
fromSign Plus = id
fromSign Minus = negate

getSign :: (Num a, Eq a) => a -> Sign
getSign x
  | signum x == (-1) = Minus
  | otherwise = Plus

-- add two bits, and a carry bit, and return a bit and the carry bit
addBits :: Bit -> Bit -> Bit -> (Bit, Bit)
addBits BZero BZero b = (b, BZero)
addBits BZero b BZero = (b, BZero)
addBits b BZero BZero = (b, BZero)
addBits BOne BOne BOne = (BOne, BOne)
addBits _ _ _ = (BZero, BOne)

-- add together a sequence of bits in order, carrying a carry bit
addBinary' :: [(Bit, Bit)] -> Bit -> [Bit]
addBinary' [] bc = [bc]
addBinary' ((b0,b1):bs) bc = b : addBinary' bs bc'
  where (b, bc') = addBits b0 b1 bc

addBinary :: [Bit] -> [Bit] -> [Bit]
addBinary as bs = dropWhile (==BZero) $ reverse (addBinary' zipped BZero)
  where as' = reverse as
        bs' = reverse bs
        zipped = zipWithDefault (\a b -> (a,b)) as' bs' BZero BZero

-- Sign is the sign of the number, Integer is the exponent, Binary is the mantissa
data PreciseFloat = PF Sign Integer Streams

toFloatingWithPrecision :: Floating a => Integer -> PreciseFloat -> a
-- toFloatingWithPrecision _ (PF _ _ (Base [])) = 0
toFloatingWithPrecision 0 (PF _ _ _) = 0
toFloatingWithPrecision i (PF s e (BS (Stream b bs))) = (fromSign s) (fromBit b) * (2**(fromInteger e)) + toFloatingWithPrecision (i-1) (PF s (e-1) (BS bs))
toFloatingWithPrecision i (PF s e (AS as)) = toFloatingWithPrecision i (PF s e (BS $ evaluateTo i as))

showAsDouble :: PreciseFloat -> String
showAsDouble pf = show $ ((toFloatingWithPrecision 64 pf) :: Double)

instance Show PreciseFloat where
  show (PF s e bs) = "PF " ++ show s ++ " " ++ show e ++ " " ++ show bs

toBinaryIntegral' :: Integral a => a -> [Bit]
toBinaryIntegral' 0 = []
toBinaryIntegral' x = toBinaryIntegral' (quot x 2) ++ [toBit x]

toBinaryIntegral :: Integral a => a -> (Sign, [Bit])
toBinaryIntegral x = (getSign x, toBinaryIntegral' $ abs x)

toBinaryFractional' :: (RealFrac a) => a -> [Bit]
toBinaryFractional' 0 = []
toBinaryFractional' x
  | intPart > 0 = BOne : toBinaryFractional' fracPart
  | otherwise = BZero : toBinaryFractional' fracPart
  where (intPart::Integer, fracPart) = properFraction (x*2)

toBinaryFractional :: (RealFrac a) => a -> (Sign,([Bit],[Bit]))
toBinaryFractional x = (getSign x, (toBinaryIntegral' intPart, toBinaryFractional' fracPart))
  where x' = abs x
        (intPart::Integer, fracPart) = properFraction x'

toPreciseFloatIntegral :: (Integral a) => a -> PreciseFloat
toPreciseFloatIntegral x = PF s (fromIntegral (length intPart) - 1) (BS $ fromListBits intPart)
  where (s, intPart) = toBinaryIntegral x

toPreciseFloat :: (RealFrac a) => a -> PreciseFloat
toPreciseFloat x
  | length intPart > 0 = PF s (fromIntegral (length intPart) - 1) (BS $ fromListBits $ intPart ++ fracPart)
  | otherwise = PF s ((- fromIntegral fracPartZeroes) - 1) (BS $ fromListBits $ drop fracPartZeroes fracPart)
  where (s, (intPart, fracPart)) = toBinaryFractional x
        fracPartZeroes = length $ takeWhile (==BZero) fracPart

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



