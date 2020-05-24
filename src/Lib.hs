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

type family Add n m where
    Add 'Zero n = n
    Add ('Succ n) m = 'Succ (Add n m)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
