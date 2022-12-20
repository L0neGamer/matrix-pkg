{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Foldable (find, toList)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import Prelude hiding (zipWith)
import GHC.TypeNats as TN
import Data.Proxy
import GHC.Natural (Natural)

type family GT (n :: Nat) (m :: Nat) where
  GT n m = And (m <=? n) (EQ n m)

type family EQ (n :: Nat) (m :: Nat) where
  EQ n m = And (n <=? m) (m <=? n)

type family Or (n :: Bool) (m :: Bool) where
  Or 'True _ = 'True
  Or _ 'True = 'True
  Or _ _ = 'False

type family And (n :: Bool) (m :: Bool) where
  And 'True 'True = 'True
  And _ _ = 'False

type family Not (n :: Bool) where
  Not 'True = 'False
  Not 'False = 'True

type family IsEven (n :: Nat) where
  IsEven n = EQ (Mod n 2) 0

-- this allows for use of inference of the exponential type
--  thanks to finnbar and dixonary!
type family RLog (n :: Nat) (m :: Nat) (x :: Nat) (i :: Nat) where
  RLog n m m i = i
  RLog n m x i = RLogBool n m x i (GT m x)

-- this is a helper type so that we can error out properly
type family RLogBool (n :: Nat) (m :: Nat) (x :: Nat) (i :: Nat) (mx :: Bool) where
  RLogBool n m x i 'True = (RLog n m (n TN.* x) (1 + i))
  RLogBool n m x i 'False =
    TypeError
      ( 'Text "Recursed too deep! Base ("
          ':<>: 'ShowType n
          ':<>: 'Text ") does not divide evenly into ("
          ':<>: 'ShowType m
          ':<>: 'Text ")"
      )

-- wrapping some stuff up in syntactic sugar
type ReverseLog n m = RLog n m n 1

-- the thing we wanna constrain on
type GetExp n i = ReverseLog n (n ^ i)

type family IsDivisibleBy (n :: Nat) (m :: Nat) where
  IsDivisibleBy _ 1 = 'True
  IsDivisibleBy n m =
    IfElse
      (EQ n m)
      'True
      (IfElse (GT n m) (IsDivisibleBy (n - m) m) 'False)

type family IfElse (b :: Bool) n m where
  IfElse 'True n _ = n
  IfElse 'False _ m = m

-- -- ways to generate minimums and maximums of Fins
-- instance (KnownNat n) => Bounded (Fin n) where
--   minBound = FZero
--   maxBound =
--     case natSing @n of
--       1 -> FZero
--       _ -> FSucc maxBound

-- converting to and from Ints for Fins
-- a bit unsafe, but that's fine, since it's
-- supposed to be
-- instance (KnownNat n) => Enum (Fin n) where
--   fromEnum = finSize
--   toEnum n = finFromInt n n (natSing :: NatS n)

-- finFromInt :: forall n m. (KnownNat n, KnownNat m) => Int -> Int -> NatS m -> Fin n
-- finFromInt 0 _ _ = FZero
-- finFromInt n o o'
--   | n > 0 =
--     case natSing @n of
--       OneS -> err
--       SuccS _ -> FSucc (finFromInt (n - 1) o o')
--   | otherwise = err
--   where
--     err = error $ "bad Int for finFromInt for Fin (" ++ show (demote (Proxy :: Proxy m)) ++ "): " ++ show o

-- define my own type class for operators between two
-- types
class LinearData v where
  (^*^) :: Num a => v a -> v a -> v a
  (^*^) = zipWith (*)
  zipWith :: (a -> b -> c) -> v a -> v b -> v c

-- thank you to dixonary at https://discord.com/channels/189453139584221185/231852430701232128/806896486860324894
getAt :: (Foldable t, Integral b) => b -> t a -> a -> a
getAt i xs def = maybe def snd $ find ((== i) . fst) $ zip [0 ..] $ toList xs

-- below are some extra functions which are just useful for testing
cantorPairing :: Integral a => a -> a -> a
cantorPairing a b = div ((a + b) * (a + b + 1)) 2 + b

inverseCantorPairing :: Integral a => a -> (a, a)
inverseCantorPairing a = (w - a + t, a - t)
  where
    w = floor (sqrt (8 * fromIntegral a + 1) - 1 / 2 :: Double)
    t = div (w ^ (2 :: Int) + w) 2

padList :: a -> Int -> [a] -> [a]
padList a i as = replicate i a ++ as
