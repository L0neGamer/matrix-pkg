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

instance Eq a => Eq (Vector n a) where
    v1 == v2 = and $ vecZipWith (==) v1 v2
    
-- -- reverse'' :: Vector ('Succ n) a -> Vector m a -> Vector ('Succ (Add n m)) a
-- -- -- reverse'' (VSingle a) vs = VCons a vs
-- -- reverse'' (VCons v vs) vs' = reverse' vs (VCons v vs')

-- -- reverse' :: Vector n a -> Vector m a -> Vector (Add n m) a
-- -- reverse' (VSingle a) vs = VCons a vs
-- -- reverse' vs vs' = reverse'' vs vs'

-- reverse'' :: Vector ('Succ n) a -> Vector m a -> Vector (Add n ('Succ m)) a
-- reverse'' (VCons a vs) vs' = reverse' vs (a .:: vs')

-- reverse' :: Vector n a -> Vector m a -> Vector (Add n m) a
-- reverse' (VSingle a) vs = a .:: vs
-- reverse' vs@(VCons a _) vs' = reverse'' vs vs'

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
vecTail _ = error "unreachable pattern in vecTail"
vecSplit :: Vector ('Succ n) a -> (a, Vector n a)
vecSplit v = (vecHead v, vecTail v)

vecZipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
vecZipWith f (VSingle a) (VSingle b) = (VSingle (f a b))
vecZipWith f (VCons a as) (VCons b bs) = f a b .:: vecZipWith f as bs
vecZipWith _ _ _ = error "unreachable pattern in vecZipWith"

setAt :: Integral a => a -> b -> Vector n b -> Vector n b
setAt 0 b (VSingle _) = VSingle b
setAt _ _ v@(VSingle _) = v
setAt 0 b (VCons _ vs) = VCons b vs
setAt n b vs'@(VCons v vs)
    | n > 0 = VCons v $ setAt (n - 1) b vs
    | otherwise = vs'

-- drops the item at index i, or the last element 
dropItem :: Integral a => a -> Vector ('Succ n) b -> Vector n b
dropItem 0 (VCons _ vs) = vs
dropItem _ (VCons a (VSingle _)) = VSingle a
dropItem i (VCons a vs@(VCons _ _)) = VCons a $ dropItem (i-1) vs
dropItem _ _ = error "unreachable pattern in dropItem"

-- set every item in a vector to a given value
setVecTo :: a -> Vector n b -> Vector n a
setVecTo a (VSingle _) = VSingle a
setVecTo a (VCons _ vs) = VCons a (setVecTo a vs)

-- apply a function to every element of the vector except the first,
--  then call this function again on the rest of the vector
applyToRest :: (a -> a) -> Vector n a -> Vector n a
applyToRest _ (VSingle a) = VSingle a
applyToRest f (VCons a as) = VCons a (applyToRest f $ fmap f as)

-- when given a vector of length n, make it a vector of increasing value
incrementingVec :: Num b => Vector n a -> Vector n b
incrementingVec = applyToRest (+1) . setVecTo 0

-- find the dot product between two vectors
dotProd :: Num a => Vector n a -> Vector n a -> a
dotProd v1 v2 = sum $ vecZipWith (*) v1 v2

-- find the cross product between two three dimensional vectors
crossProd :: Num a => Vector Three a -> Vector Three a -> Vector Three a
crossProd (VCons ax (VCons ay (VSingle az))) (VCons bx (VCons by (VSingle bz))) = VCons (ay*bz-az*by) (VCons (az*bx-ax*bz) (VSingle (ax*by-ay*bx)))
crossProd _ _ = error "unreachable pattern in crossProd"
