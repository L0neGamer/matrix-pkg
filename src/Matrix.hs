{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Matrix where

import GHC.Generics

-- thanks to https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html


data Length = Length Int

data Nat = Succ Nat | Zero deriving (Show, Eq)

type family Add n m where
    Add 'Zero n = n
    Add ('Succ n) m = 'Succ (Add n m)

data Vector (n :: Nat) a where
    VNil :: Vector Zero a
    VCons :: a -> Vector n a -> Vector (Succ n) a

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil v = v
append (VCons a rest) vs = VCons a (append rest vs)

binOpVec :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
binOpVec _ VNil _ = VNil
binOpVec f (VCons a as) (VCons b bs) = VCons (f a b) (binOpVec f as bs)

type Matrix2 n m a = Vector n (Vector m a)
-- n=rows, m=cols

applyNTimes :: Integral a => a -> (b -> b) -> b -> b
applyNTimes i f x
    | i < 1 = x
    | otherwise = applyNTimes (i - 1) f (f x)

constructMatrix n m a = applyNTimes n (VCons row) VNil
    where row = applyNTimes m (VCons a) VNil
-- squareMatrix2 a = VCons row $ VCons row $ VNil
--     where row = VCons a $ VCons a $ VNil

instance Show a => Show (Vector n a) where
    show VNil         = "VNil"
    show (VCons a as) = "VCons (" ++ show a ++ ") (" ++ show as ++ ")"
