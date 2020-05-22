{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

data Nat = Succ Nat | One deriving (Show, Eq)
type Two = 'Succ One
type Three = 'Succ Two
type Four = 'Succ Three
type Five = 'Succ Four

type family Add n m where
    Add 'One n = 'Succ n
    Add ('Succ n) m = 'Succ (Add n m)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
