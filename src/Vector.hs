{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Vector where

import           Data.Foldable (toList)
import           Data.Maybe    (fromJust)
import           Lib

data Vector (n :: Nat) a where
  VSingle :: a -> Vector One a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Functor (Vector n) where
  fmap f (VSingle a)  = VSingle (f a)
  fmap f (VCons a vs) = VCons (f a) (fmap f vs)

instance Foldable (Vector n) where
  foldr f z (VSingle a)  = a `f` z
  foldr f z (VCons a vs) = a `f` foldr f z vs

instance Show a => Show (Vector n a) where
  show vs = "Vector " ++ show (toList vs)

instance Eq a => Eq (Vector n a) where
  v1 == v2 = and $ vecZipWith (==) v1 v2

instance Traversable (Vector n) where
  traverse f (VSingle a)  = VSingle <$> f a
  traverse f (VCons a vs) = VCons <$> f a <*> traverse f vs

reverse :: Vector n a -> Vector n a
reverse vs = fromJust $ fromList (Prelude.reverse (toList vs)) vs

-- takes a list that has size equal to the given vector,
--  and constructs a vector from the list items
fromList :: [a] -> Vector n b -> Maybe (Vector n a)
fromList [a] (VSingle _)     = Just $ VSingle a
fromList (a:as) (VCons _ vs) = fromList as vs >>= Just . VCons a
fromList _ _                 = Nothing

-- get the size of the vector
size :: Vector n a -> Integer
size (VSingle _)  = 1
size (VCons _ vs) = 1 + size vs

-- easy constructor for vector concatenation
(.::) :: a -> Vector n a -> Vector ('Succ n) a
(.::) = VCons

infixr .::

-- for unified construction
singleton :: a -> Vector One a
singleton = VSingle

-- adds an item to the end of a vector
appendVal :: a -> Vector n a -> Vector ('Succ n) a
appendVal a (VSingle b)  = b .:: singleton a
appendVal a (VCons b bs) = b .:: appendVal a bs

-- adds a vector to the end of a vector
append :: Vector n a -> Vector m a -> Vector (Add n m) a
append (VSingle a) vs    = a .:: vs
append (VCons a rest) vs = a .:: append rest vs

-- get parts of a vector
vecHead :: Vector n a -> a
vecHead (VSingle a) = a
vecHead (VCons a _) = a

vecTail :: Vector ('Succ n) a -> Vector n a
vecTail (VCons _ vs) = vs
vecTail _            = error "unreachable pattern in vecTail"

vecSplit :: Vector ('Succ n) a -> (a, Vector n a)
vecSplit v = (vecHead v, vecTail v)

-- zip together two vectors
vecZipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
vecZipWith f (VSingle a) (VSingle b) = (VSingle (f a b))
vecZipWith f (VCons a as) (VCons b bs) = f a b .:: vecZipWith f as bs
vecZipWith _ _ _ = error "unreachable pattern in vecZipWith"

-- set an item in a vector. if it is out of range, return Nothing
setAt :: Integral a => a -> b -> Vector n b -> Maybe (Vector n b)
setAt 0 b (VSingle _)  = Just $ VSingle b
setAt _ _ (VSingle _)  = Nothing
setAt 0 b (VCons _ vs) = Just $ VCons b vs
setAt n b (VCons v vs) = setAt (n - 1) b vs >>= Just . VCons v

-- get an item in a vector. if it is out of range, return Nothing
getAt :: Integral a => a -> Vector n b -> Maybe b
getAt 0 vs           = Just $ vecHead vs
getAt n (VCons _ vs) = getAt (n - 1) vs
getAt _ _            = Nothing

-- drops the item at index i. if it is out of range, return Nothing
dropItem :: Integral a => a -> Vector ('Succ n) b -> Maybe (Vector n b)
dropItem 0 (VCons _ vs)             = Just vs
dropItem 1 (VCons a (VSingle _))    = Just $ VSingle a
dropItem _ (VCons _ (VSingle _))    = Nothing
dropItem i (VCons a vs@(VCons _ _)) = dropItem (i - 1) vs >>= Just . VCons a
dropItem _ _                        = error "unreachable pattern in dropItem"

-- set every item in a vector to a given value
setVecTo :: a -> Vector n b -> Vector n a
setVecTo a = fmap (\_ -> a)

-- apply a function to every element of the vector except the first,
--  then call this function again on the rest of the vector
applyToRest :: (a -> a) -> Vector n a -> Vector n a
applyToRest _ (VSingle a)  = VSingle a
applyToRest f (VCons a as) = VCons a (applyToRest f $ fmap f as)

-- when given a vector of length n, make it a vector of increasing value
incrementingVec :: Num b => Vector n a -> Vector n b
incrementingVec = applyToRest (+ 1) . setVecTo 0

-- find the dot product between two vectors
dotProd :: Num a => Vector n a -> Vector n a -> a
dotProd v1 v2 = sum $ vecZipWith (*) v1 v2

-- find the cross product between two three dimensional vectors
-- cheats by just applying the addition as opposed to any other thing
crossProd :: Num a => Vector Three a -> Vector Three a -> Vector Three a
crossProd (VCons ax (VCons ay (VSingle az))) (VCons bx (VCons by (VSingle bz))) =
  VCons
    (ay * bz - az * by)
    (VCons (az * bx - ax * bz) (VSingle (ax * by - ay * bx)))
crossProd _ _ = error "unreachable pattern in crossProd"
