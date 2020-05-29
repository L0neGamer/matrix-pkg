{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}

module Lib where

import           Data.Kind

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

instance Bounded (Fin 'One) where
  minBound = FZero
  maxBound = FZero

instance Bounded (Fin n) => Bounded (Fin ('Succ n)) where
  minBound = FZero
  maxBound = FSucc (maxBound)

instance Enum (Fin 'One) where
  fromEnum = fromIntegral . finSize
  toEnum 1 = FZero
  toEnum _ = error "bad argument"

instance (Enum (Fin n)) => Enum (Fin ('Succ n)) where
  fromEnum = fromIntegral . finSize
  toEnum 1 = FZero
  toEnum n
    | n > 1 = FSucc (toEnum (n - 1))
    | otherwise = error "bad argument"

class LinearData v where
  (^*^) :: Num a => (v a) -> (v a) -> (v a)
  zipWith :: (a -> b -> c) -> (v a) -> (v b) -> (v c)

finSize :: Fin n -> Integer
finSize FZero     = 1
finSize (FSucc f) = 1 + finSize f

cantorPairing :: Integral a => a -> a -> a
cantorPairing a b = div ((a + b) * (a + b + 1)) 2 + b

inverseCantorPairing :: Integral a => a -> (a, a)
inverseCantorPairing a = (w - a + t, a - t)
  where
    w = floor (sqrt (8 * fromIntegral a + 1) - 1 / 2 :: Double)
    t = div (w ^ (2 :: Int) + w) 2

padList :: a -> Int -> [a] -> [a]
padList a i as = replicate i a ++ as
