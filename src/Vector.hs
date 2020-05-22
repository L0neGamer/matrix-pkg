{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Vector where

import Lib

data Vector (n :: Nat) a where
    VNil :: Vector Zero a
    VCons :: a -> Vector n a -> Vector (Succ n) a

instance Functor (Vector n) where
    fmap _ VNil = VNil
    fmap f (VCons a vs) = VCons (f a) (fmap f vs)

instance Foldable (Vector n) where
    foldr f z VNil = z
    foldr f z (VCons a vs) = a `f` foldr f z vs

instance Show a => Show (Vector n a) where
    show VNil         = "VNil"
    show (VCons a as) = "VCons "++ a' ++ " " ++ as'
        where showa = show a
              a' | ' ' `elem` showa = "(" ++ showa ++ ")"
                 | otherwise = showa
              showas = show as
              as' | ' ' `elem` showas = "(" ++ showas ++ ")"
                  | otherwise = showas


singleton :: a -> Vector One a
singleton a = VCons a VNil

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil v = v
append (VCons a rest) vs = VCons a (append rest vs)

vecHead :: Vector (Succ n) a -> a
vecHead (VCons a _) = a
vecTail' :: Vector (Succ n) a -> Vector n a
vecTail' (VCons _ vs) = vs

vecTail :: Vector n a -> Vector (Dec n) a
vecTail VNil = VNil
vecTail v@(VCons _ _) = vecTail' v

binOpVec :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
binOpVec _ VNil _ = VNil
binOpVec f (VCons a as) (VCons b bs) = VCons (f a b) (binOpVec f as bs)
