{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies, UndecidableInstances, TypeOperators, TypeFamilyDependencies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- {-# RankNTypes #-}
module Lib where

data Nat = Succ Nat | Zero deriving (Show, Eq)
type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two
type Four = 'Succ Three
type Five = 'Succ Four

--  look at https://wiki.haskell.org/Type_arithmetic
-- http://archive.fo/JwMNI
data Z = Z
data S a = S a

class Sum2 a b c | a b -> c, a c -> b
instance Sum2 Z a a
instance Sum2 a b c => Sum2 (S a) b (S c)
class Sum a b c | a b -> c, a c -> b, b c -> a
instance (Sum2 a b c, Sum2 b a c) => Sum a b c

data Elems n a where
    Elem :: a -> Elems (S Z) a
    MoreElem :: a -> Elems n a -> Elems (S n) a

add :: Sum a b c => a -> b -> c
add = undefined
zero = undefined :: Z
one = undefined :: S Z
two = add one one

type family Add n m where
    Add 'Zero n = n
    Add ('Succ n) m = 'Succ (Add n m)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
