{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}

module Lib where

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.TH

$(singletons
    [d|

  data Nat = One
           | Succ Nat
  |])

deriving instance Show Nat

deriving instance Eq Nat

type Two = 'Succ 'One

type Three = 'Succ Two

type Four = 'Succ Three

type Five = 'Succ Four

--  look at https://wiki.haskell.org/Type_arithmetic
-- http://archive.fo/JwMNI
type family Add n m where
  Add 'One n = 'Succ n
  Add ('Succ n) m = 'Succ (Add n m)

singSize :: Sing (n :: Nat) -> Integer
singSize (SOne)    = 1
singSize (SSucc s) = 1 + singSize s

data Fin :: Nat -> Type where
  FZero :: Fin n
  FSucc :: Fin n -> Fin ('Succ n)

deriving instance Show (Fin n)

deriving instance Eq (Fin n)

finSize :: Integral a => Fin n -> a
finSize FZero     = 1
finSize (FSucc f) = 1 + finSize f

cantorPairing :: Integral a => a -> a -> a
cantorPairing a b = div ((a + b) * (a + b + 1)) 2 + b

inverseCantorPairing :: Integral a => a -> (a, a)
inverseCantorPairing a = (w - a + t, a - t)
  where
    w = floor ((((sqrt (8 * (fromIntegral a) + 1)) - 1) / 2) :: Double)
    t = (div (w ^ (2 :: Int) + w) 2)

padList :: a -> Int -> [a] -> [a]
padList a i as = replicate i a ++ as
