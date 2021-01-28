module Lib where

import           Data.Kind
import           Prelude hiding (zipWith)
import Data.AdditiveGroup

{-
Nat is for keeping the size of the vector in the type.
this is done via promotion via... DataKinds I think
Think of 'Nat and friends as the Types of this setup

:k Nat   == Nat :: *
:k One   == One :: Nat
:t One   == One :: Nat
:k Succ  == Succ :: Nat -> Nat
:t Succ  == Succ :: Nat -> Nat
So the above definitions show that a Nat is a plain type; it takes no arguments
-}
data Nat
  = One
  | Succ Nat
  deriving (Show, Eq)

-- Following are KnownNats, which help with automatic construction of things
data NatS :: Nat -> Type where
  OneS :: NatS 'One
  SuccS :: KnownNat n => NatS n -> NatS ('Succ n)

deriving instance Show (NatS a)
deriving instance Eq (NatS a)

class KnownNat (n :: Nat) where
  natSing :: NatS n

instance KnownNat 'One where
  natSing = OneS

instance KnownNat n => KnownNat ('Succ n) where
  natSing = SuccS natSing

-- Fin is for indexing through the data structures.
-- think of it as the "Numbers" of these structures
-- :k Fin   == Fin :: Nat -> *
-- :k FZero == FZero :: Fin n
-- :t FZero == FZero :: Fin n
-- :k FSucc == FSucc :: Fin n -> Fin ('Succ n)
-- :t FSucc == FSucc :: Fin n -> Fin ('Succ n)
data Fin :: Nat -> Type where
  FZero :: Fin n
  FSucc :: Fin n -> Fin ('Succ n)

deriving instance Show (Fin n)

deriving instance Eq (Fin n)

deriving instance Ord (Fin n)

type One = 'One

type Two = 'Succ 'One

type Three = 'Succ Two

type Four = 'Succ Three

type Five = 'Succ Four

type Six = 'Succ Five

type Seven = 'Succ Six

type Eight = 'Succ Seven

type Nine = 'Succ Eight

type Ten = 'Succ Nine

--  look at https://wiki.haskell.org/Type_arithmetic
-- http://archive.fo/JwMNI
type family Add n m where
  Add 'One n = 'Succ n
  Add ('Succ n) m = 'Succ (Add n m)

-- Multiplication, allowing ever more powerful operators
type family Mul n m where
  Mul 'One m = m
  Mul ('Succ n) m = Add m (Mul n m)

-- ways to generate minimums and maximums of Fins
instance (KnownNat n) => Bounded (Fin n) where
  minBound = FZero
  maxBound =
    case natSing @n of
      OneS    -> FZero
      SuccS _ -> FSucc maxBound

-- converting to and from Ints for Fins
-- a bit unsafe, but that's fine, since it's 
-- supposed to be
instance (KnownNat n) => Enum (Fin n) where
  fromEnum = finSize
  toEnum 1 = FZero
  toEnum n
    | n > 1 =
      case natSing @n of
        OneS    -> err
        SuccS _ -> FSucc (toEnum (n - 1))
    | otherwise = err
    where
      err = error $ "bad Int for toEnum in Finn: " ++ show n

-- define my own type class for operators between two 
-- types
class LinearData v where
  (^*^) :: Num a => (v a) -> (v a) -> (v a)
  (^*^) = zipWith (*)
  zipWith :: (a -> b -> c) -> (v a) -> (v b) -> (v c)

-- get the numerical representation of a Fin
finSize :: Num a => Fin n -> a
finSize FZero     = 1
finSize (FSucc f) = 1 + finSize f

-- create a list of Fins equal to the size of the type in 
-- ascending order
fins :: forall n . KnownNat n
  => [Fin n]
fins = map toEnum $ take (fromEnum (maxBound :: Fin n)) [1,2 ..]

-- get an ascending list of Fins greater than the given Fin
finFrom :: forall n . KnownNat n
  => Fin n
  -> [Fin n]
finFrom from = dropWhile (< from) fins

-- convert a NatS into a number
natSSize :: Num a => NatS n -> a
natSSize OneS      = 1
natSSize (SuccS s) = 1 + natSSize s

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
