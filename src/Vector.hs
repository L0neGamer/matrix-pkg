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
    VSingle :: a -> Vector One a
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

-- reverse'' :: Vector ('Succ n) a -> Vector m a -> Vector (Add n ('Succ m)) a
-- -- reverse'' (VSingle a) vs = VCons a vs
-- reverse'' (VCons v vs) vs' = reverse' vs (VCons v vs')

-- reverse' :: Vector n a -> Vector m a -> Vector (Add n m) a
-- reverse' (VSingle a) vs = VCons a vs
-- reverse' vs vs' = reverse'' vs vs'

-- reverse :: Vector n a -> Vector n a
-- reverse (VSingle a) = VSingle a
-- reverse (VCons v vs) = reverse' vs (VSingle v)

showVector :: Show a => Vector n a -> String
showVector = show . toList

size :: Vector n a -> Integer
size (VSingle _) = 1
size (VCons _ vs) = 1 + size vs

(.::) :: a -> Vector n a -> Vector ('Succ n) a
a .:: v = VCons a v
infixr .::

singleton :: a -> Vector One a
singleton = VSingle

appendVal :: a -> Vector n a -> Vector ('Succ n) a
appendVal a (VSingle b) = b .:: singleton a
appendVal a (VCons b bs) = b .:: appendVal a bs

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append (VSingle a) vs = a .:: vs
append (VCons a rest) vs = a .:: append rest vs

vecHead :: Vector n a -> a
vecHead (VSingle a) = a
vecHead (VCons a _) = a
vecTail :: Vector ('Succ n) a -> Vector n a
vecTail (VCons _ vs) = vs
vecSplit :: Vector ('Succ n) a -> (a, Vector n a)
vecSplit v = (vecHead v, vecTail v)

vecZipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
vecZipWith f (VSingle a) (VSingle b) = (VSingle (f a b))
vecZipWith f (VCons a as) (VCons b bs) = f a b .:: vecZipWith f as bs

setAt :: Integral a => a -> b -> Vector n b -> Vector n b
setAt 0 b (VSingle _) = VSingle b
setAt _ _ v@(VSingle _) = v
setAt 0 b (VCons _ vs) = VCons b vs
setAt n b vs'@(VCons v vs)
    | n > 0 = VCons v $ setAt (n - 1) b vs
    | otherwise = vs'
