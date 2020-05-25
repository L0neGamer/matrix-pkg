{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- {-# RankNTypes #-}
module Lib where

data Nat
  = Succ Nat
  | Zero
  deriving (Show, Eq)

type One = 'Succ 'Zero

type Two = 'Succ One

type Three = 'Succ Two

type Four = 'Succ Three

type Five = 'Succ Four

--  look at https://wiki.haskell.org/Type_arithmetic
-- http://archive.fo/JwMNI
type family Add n m where
  Add 'Zero n = n
  Add ('Succ n) m = 'Succ (Add n m)

cantorPairing :: Integral a => a -> a -> a
cantorPairing a b = div ((a + b) * (a + b + 1)) 2 + b

inverseCantorPairing :: Integral a => a -> (a, a)
inverseCantorPairing a = (w - a + t, a - t)
  where
    w = floor ((((sqrt (8 * (fromIntegral a) + 1)) - 1) / 2) :: Double)
    t = (div (w ^ (2 :: Int) + w) 2)

padList :: a -> Int -> [a] -> [a]
padList a i as = replicate i a ++ as
