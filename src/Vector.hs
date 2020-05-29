{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wall                       #-}

module Vector where

import           Control.Applicative
import           Data.AdditiveGroup
import           Data.Foldable       (toList)
import           Data.Maybe          (fromJust)
import           Data.VectorSpace
import           Lib

{-
old help:

-- https://stackoverflow.com/questions/49206636/how-to-make-fixed-length-vectors-instance-of-applicative

-- https://stackoverflow.com/questions/5802628/monad-instance-of-a-number-parameterised-vector
-}

data Vector (n :: Nat) a where
  VecSing :: a -> Vector 'One a
  (:+) :: a -> Vector n a -> Vector ('Succ n) a
infixr 8 :+
deriving instance Show a => Show (Vector n a)

instance Functor (Vector n) where
  fmap f (VecSing a) = VecSing (f a)
  fmap f (a :+ vs)   = f a :+ fmap f vs

instance Foldable (Vector n) where
  foldr f z (VecSing a) = a `f` z
  foldr f z (a :+ vs)   = a `f` foldr f z vs

instance Eq a => Eq (Vector n a) where
  v1 == v2 = and $ vecZipWith (==) v1 v2

instance Traversable (Vector n) where
  traverse f (VecSing a) = VecSing <$> f a
  traverse f (a :+ vs)   = (:+) <$> f a <*> traverse f vs

instance Ord a => Ord (Vector n a) where
  (VecSing a) <= (VecSing b) = a <= b
  (a :+ as) <= (b :+ bs)
    | a < b = True
    | a > b = False
    | otherwise = as <= bs

-- https://stackoverflow.com/questions/62039392/how-do-i-allow-one-constraint-to-imply-another-in-haskell/62040229#62040229
instance KnownNat n => Applicative (Vector n) where
  pure a = case natSing @n of
    OneS  -> VecSing a
    SuccS -> a :+ pure a
  (<*>) = case natSing @n of
    OneS  -> \(VecSing f) (VecSing a) -> VecSing $ f a
    SuccS -> \(f :+ fs) (a :+ as) -> f a :+ (fs <*> as)

instance KnownNat n => Monad (Vector n) where
  (>>=) = case natSing @n of
    OneS  -> \(VecSing a) f -> f a
    SuccS -> \(a :+ as) f -> (vecHead $ f a) :+ (as >>= (vecTail . f))

instance (Num a, KnownNat n) => AdditiveGroup (Vector n a) where
  zeroV = pure 0
  negateV = fmap negate
  (^+^) = liftA2 (+)

instance (Num a, KnownNat n) => VectorSpace (Vector n a) where
  type Scalar (Vector n a) = a
  a *^ b = fmap (a*) b

replicate :: forall a n . KnownNat n => a -> Vector n a
replicate = case natSing @n of
  OneS -> \a -> VecSing a
  SuccS -> \a -> a :+ Vector.replicate a

generate :: forall a n . KnownNat n => (Fin n -> a) -> Vector n a
generate f = case natSing @n of
  OneS -> VecSing (f FZero)
  SuccS -> f FZero :+ generate (f . FSucc)

reverse :: Vector n a -> Vector n a
reverse vs = fromJust $ fromList (Prelude.reverse (toList vs)) vs

-- takes a list that has size equal to the given vector,
--  and constructs a vector from the list items
fromList :: [a] -> Vector n b -> Maybe (Vector n a)
fromList [a] (VecSing _)  = Just $ VecSing a
fromList (a:as) (_ :+ vs) = fromList as vs >>= Just . (:+) a
fromList _ _              = Nothing

-- for unified construction
singleton :: a -> Vector 'One a
singleton = VecSing

-- adds an item to the end of a vector
appendVal :: a -> Vector n a -> Vector ('Succ n) a
appendVal a (VecSing b) = b :+ singleton a
appendVal a (b :+ bs)   = b :+ appendVal a bs

-- adds a vector to the end of a vector
(+++) :: Vector n a -> Vector m a -> Vector (Add n m) a
(VecSing a) +++ vs = a :+ vs
(a :+ rest) +++ vs = a :+ rest +++ vs

-- get parts of a vector
vecHead :: Vector n a -> a
vecHead (VecSing a) = a
vecHead (a :+ _)    = a

vecTail :: Vector ('Succ n) a -> Vector n a
vecTail (_ :+ vs) = vs

vecSplit :: Vector ('Succ n) a -> (a, Vector n a)
vecSplit v = (vecHead v, vecTail v)

-- zip together two vectors
vecZipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
vecZipWith f (VecSing a) (VecSing b) = VecSing (f a b)
vecZipWith f (a :+ as) (b :+ bs)     = f a b :+ vecZipWith f as bs

dropIndex :: Fin ('Succ n) -> Vector ('Succ n) a -> Vector n a
dropIndex FZero (_ :+ vs)                  = vs
dropIndex (FSucc FZero) (b :+ (VecSing _)) = VecSing b
dropIndex (FSucc fin) (b :+ vs@(_ :+ _))   = b :+ dropIndex fin vs

replace :: Fin n -> a -> Vector n a -> Vector n a
replace FZero a (VecSing _)                = VecSing a
replace FZero a (_ :+ vs)                  = a :+ vs
replace (FSucc FZero) a (b :+ (VecSing _)) = b :+ VecSing a
replace (FSucc fin) a (b :+ vs)            = b :+ replace fin a vs

index :: Fin n -> Vector n a -> a
index FZero vs            = vecHead vs
index (FSucc i) (_ :+ vs) = index i vs

-- set every item in a vector to a given value
setVecTo :: a -> Vector n b -> Vector n a
setVecTo a = fmap (const a)

-- apply a function to every element of the vector except the first,
--  then call this function again on the rest of the vector
applyToRest :: (a -> a) -> Vector n a -> Vector n a
applyToRest _ (VecSing a) = VecSing a
applyToRest f (a :+ as)   = a :+ applyToRest f (fmap f as)

-- when given a vector of length n, make it a vector of increasing value
incrementingVec :: Num b => Vector n a -> Vector n b
incrementingVec = applyToRest (+ 1) . setVecTo 0

-- find the dot product between two vectors
dotProd :: Num a => Vector n a -> Vector n a -> a
dotProd v1 v2 = sum $ vecZipWith (*) v1 v2

-- find the cross product between two three dimensional vectors
-- cheats by just applying the addition as opposed to any other thing
crossProd :: Num a => Vector Three a -> Vector Three a -> Vector Three a
crossProd (ax :+ (ay :+ (VecSing az))) (bx :+ (by :+ (VecSing bz))) =
  (ay * bz - az * by) :+ ((az * bx - ax * bz) :+ VecSing (ax * by - ay * bx))

(.*) :: Num a => Vector n a -> Vector n a -> Vector n a
(.*) = vecZipWith (*)
