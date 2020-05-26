{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}

module Lib where

import           Data.Kind
import           Data.Singletons.TH

{-
data SNat (a :: Nat) where
  SOne :: SNat 'One
  SSucc :: Sing n -> SNat ('Succ n)
automatically construct singletons for Nat, of "SOne" and "SSucc"
Nat is for keeping the size of the vector in the type.
this is done via promotion via... DataKinds I think
Think of 'Nat and friends as the Types of this setup

:k Nat   == Nat :: *
:k One   == One :: Nat
:t One   == One :: Nat
:k Succ  == Succ :: Nat -> Nat
:t Succ  == Succ :: Nat -> Nat
So the above definitions show that a Nat is a plain type; it takes no arguments

:k SNat  == SNat :: Nat -> *
:k SOne  == SOne :: SNat 'One
:t SOne  == SOne :: SNat 'One
:k SSucc == SSucc :: Sing n -> SNat ('Succ n)
:t SSucc == SSucc :: SNat n -> SNat ('Succ n)
the above definitions here show that SNat is a higher order type; that is,
when given a Nat, it produces a type.
-}
$(singletons
    [d|

  data Nat = One
           | Succ Nat
               deriving (Show, Eq)
  |])

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

singSize :: Sing (n :: Nat) -> Integer
singSize SOne      = 1
singSize (SSucc s) = 1 + singSize s

finSize :: Fin n -> Integer
finSize FZero     = 1
finSize (FSucc f) = 1 + finSize f

sizeDif' :: Sing n -> Fin n -> Integer
sizeDif' s f = singSize s - finSize f

sizeDif :: SingI n => Fin n -> Integer
sizeDif = sizeDif' sing

cantorPairing :: Integral a => a -> a -> a
cantorPairing a b = div ((a + b) * (a + b + 1)) 2 + b

inverseCantorPairing :: Integral a => a -> (a, a)
inverseCantorPairing a = (w - a + t, a - t)
  where
    w = floor (sqrt (8 * fromIntegral a + 1) - 1 / 2 :: Double)
    t = div (w ^ (2 :: Int) + w) 2

padList :: a -> Int -> [a] -> [a]
padList a i as = replicate i a ++ as
