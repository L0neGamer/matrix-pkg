{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Vector where

import Lib
import Data.Foldable (toList)

data Vector (n :: Nat) a where
    VSingle :: a -> Vector 'One a
    VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Functor (Vector n) where
    fmap f (VSingle a) = VSingle (f a)
    fmap f (VCons a vs) = VCons (f a) (fmap f vs)

instance Foldable (Vector n) where
    foldr f z (VSingle a) = a `f` z
    foldr f z (VCons a vs) = a `f` foldr f z vs

instance Show a => Show (Vector n a) where
    show (VSingle a)  = "VSingle " ++ show a
    show (VCons a as) = "VCons "++ a' ++ " " ++ as'
        where showa = show a
              a' | ' ' `elem` showa = "(" ++ showa ++ ")"
                 | otherwise = showa
              showas = show as
              as' | ' ' `elem` showas = "(" ++ showas ++ ")"
                  | otherwise = showas

showVector :: Show a => Vector n a -> String
showVector = show . toList

size :: Vector n a -> Integer
size (VSingle _) = 1
size (VCons _ vs) = 1 + size vs

(.::) :: a -> Vector n a -> Vector ('Succ n) a
a .:: v = VCons a v
infixr .::

singleton :: a -> Vector 'One a
singleton = VSingle

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append (VSingle a) vs = a .:: vs
append (VCons a rest) vs = a .:: append rest vs

vecHead :: Vector n a -> a
vecHead (VSingle a) = a
vecHead (VCons a _) = a
vecTail :: Vector ('Succ n) a -> Vector n a
vecTail (VCons _ vs) = vs

binOpVec :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
binOpVec f (VSingle a) (VSingle b) = (VSingle (f a b))
binOpVec f (VCons a as) (VCons b bs) = f a b .:: binOpVec f as bs
