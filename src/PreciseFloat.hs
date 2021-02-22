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

genericBitOp :: (Bit -> Bit -> Bool) -> Bits n -> Bits n -> Bits n
genericBitOp f b0s b1s = Lib.zipWith applyF b0s b1s
 where
  applyF b0 b1 | f b0 b1   = BOne
               | otherwise = BZero

makeBits :: (KnownNat n) => String -> Bits n
makeBits xs = vecFromListWithDefault BZero (fmap conv xs)
 where
  conv '0' = BZero
  conv '1' = BOne
  conv b   = error $ "non-binary number in binary string: " ++ [b]

flipBit :: Bit -> Bit
flipBit b = toBit ((fromBit b + 1) :: Int)

notBitOp :: Bits n -> Bits n
notBitOp = fmap flipBit

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

noLeadingZeroes' :: Bits n -> Integer -> (Bits n, Integer)
noLeadingZeroes' bs i
  | vecHead bs == BOne = (bs, i)
  | otherwise          = noLeadingZeroes' (bitShiftLeft' BZero bs) (i - 1)

noLeadingZeroes :: Bits n -> (Bits n, Integer)
noLeadingZeroes bs = noLeadingZeroes' bs 0

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

addBinary' :: Bits n -> Bits n -> Bits n
addBinary' x y | isZero    = x
               | otherwise = addBinary' x' y'
 where
  isZero = foldr (\b acc -> (b == BZero) && acc) True y
  carry  = genericBitOp (\b0 b1 -> b0 == b1 && b0 == BOne) x y
  x'     = genericBitOp (/=) x y
  y'     = bitShiftLeft' BZero carry

addBinary :: Bits n -> Bits n -> Bits ( 'Succ n)
addBinary x y = addBinary' (BZero :+ x) (BZero :+ y)

doAddition :: Bits n -> Bits n -> (Bits n, Sign, Integer)
doAddition b0s b1s | vecHead res == BOne = (vecDropLast res, Plus, 1)
                   | otherwise           = (res', Plus, exp')
 where
  res          = addBinary b0s b1s
  (res', exp') = noLeadingZeroes (vecTail res)

subBinary' :: Bits n -> Bits n -> Bits n
subBinary' x y | isZero    = x
               | otherwise = subBinary' x' y'
 where
  isZero = foldr (\b acc -> (b == BZero) && acc) True y
  borrow = genericBitOp (\b0 b1 -> b0 == b1 && b1 == BOne) (notBitOp x) y
  x'     = genericBitOp (/=) x y
  y'     = bitShiftLeft' BZero borrow

subBinary :: Bits n -> Bits n -> Bits ( 'Succ n)
subBinary x y = subBinary' (BZero :+ x) (BZero :+ y)

doSubtraction :: Bits n -> Bits n -> (Bits n, Sign, Integer)
doSubtraction b0s b1s
  | vecHead res == BOne = (fst flippedNoZeroes, Minus, snd flippedNoZeroes)
  | otherwise           = (fst noZeroesRes, Plus, snd noZeroesRes)
 where
  res         = subBinary b0s b1s
  noZeroesRes = noLeadingZeroes (vecTail res)
  flipped     = vecTail $ notBitOp res
  (addBits, _, addExp) =
    doAddition flipped (vecReplaceLast BOne $ fmap (\_ -> BZero) flipped)
  flippedNoZeroes = noLeadingZeroes' addBits addExp

-- Sign is the sign of the number, Integer is the exponent, Binary is the mantissa
data PreciseFloat n = PF {sign :: Sign, expo :: Integer, bits :: (Bits n)}

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

ieee754Rounding :: Bits (Add Three n) -> Bool
ieee754Rounding (_:+lastBits@(_:+(_:+(VecSing d))))
  | d == BOne && lastBits >= (makeBits "100" :: Bits Three) = True
  | d == BZero && lastBits >= (makeBits "101" :: Bits Three) = True
  | otherwise = False
ieee754Rounding (_:+bs@(_:+(_:+(_:+_)))) = ieee754Rounding bs

-- ieee754RoundNumber :: Bits (Add Three n) -> Bits n
-- ieee754RoundNumber bs
--   | doRound = undefined
--   | otherwise = 
--   where doRound = ieee754Rounding bs

-- https://cs.stackexchange.com/questions/91510/how-to-add-ieee-754-floating-point-numbers
addPF :: PreciseFloat n -> PreciseFloat n -> PreciseFloat n
addPF (PF s1 e1 bs1) (PF s2 e2 bs2) = undefined --PF s' (maxExp+expDif) bs
 where
  appendZeroes bs = appendVal BZero $ appendVal BZero $ appendVal BZero bs
  maxExp = max e1 e2
  bs1'   = bitShiftRight (maxExp - e1) BZero $ appendZeroes bs1
  bs2'   = bitShiftRight (maxExp - e2) BZero $ appendZeroes bs2
  (bs, s, expDif) | s1 == s2    = doAddition bs1' bs2'
                  | s2 == Minus = doSubtraction bs1' bs2'
                  | otherwise   = doSubtraction bs2' bs1'
  s' = if s1 == s2 && s1 == Minus then Minus else s


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

four :: Bits Four
four = makeBits "0100"

seven :: Bits Four
seven = makeBits "0111"

three :: Bits Four
three = makeBits "0011"

zero :: Bits Four
zero = makeBits ""
one :: Bits Four
one = makeBits "0001"

ten :: Bits Four
ten = makeBits "1010"
