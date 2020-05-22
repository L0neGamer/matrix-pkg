{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

data Nat = Succ Nat | Zero deriving (Show, Eq)
type One = Succ Zero
type Two = Succ One
type Three = Succ Two

type family Dec n where
    Dec Zero = Zero
    Dec (Succ n) = n

type family Add n m where
    Add 'Zero n = n
    Add ('Succ n) m = 'Succ (Add n m)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
