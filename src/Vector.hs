module Vector where

import Data.AdditiveGroup (AdditiveGroup (negateV, zeroV, (^+^)))
import Data.VectorSpace (InnerSpace (..), VectorSpace (..))
import Lib (LinearData(..))
--   ( Add,
--     Fin (..),
--     KnownNat (..),
--     LinearData (..),
--     Mul,
--     Nat (Succ),
--     NatS (OneS, SuccS),
--     One,
--     Three,
--     finSize,
--   )
import Prelude hiding (reverse, zipWith)
import GHC.TypeNats
import Fin

-- below is a useful link for vector stuff
-- https://stackoverflow.com/questions/62039392/how-do-i-allow-one-constraint-to-imply-another-in-haskell/62040229#62040229

-- create the vector data type
-- the prepend operator increases the vector size by one
data Vector (n :: Nat) a where
  VNil :: Vector 0 a
  (:+) :: a -> Vector n a -> Vector (n + 1) a

infixr 8 :+

deriving instance Show a => Show (Vector n a)

-- we should definitely define LinearData for Vectors as it's very useful
instance LinearData (Vector n) where
  zipWith f VNil VNil = VNil
  zipWith f VNil (b :+ bs) = VNil
  zipWith f (a :+ as) VNil = VNil
  zipWith f (a :+ as) (b :+ bs) = f a b :+ zipWith f as bs

instance Functor (Vector n) where
  fmap f VNil = VNil
  fmap f (a :+ vs) = f a :+ fmap f vs

instance Foldable (Vector n) where
  foldr f z VNil = z
  foldr f z (a :+ vs) = a `f` foldr f z vs

instance Eq a => Eq (Vector n a) where
  v1 == v2 = and $ zipWith (==) v1 v2

instance Traversable (Vector n) where
  traverse f VNil = VNil
  traverse f (a :+ vs) = (:+) <$> f a <*> traverse f vs

-- we can say that ordering is a thing we can do
instance Ord a => Ord (Vector n a) where
  VNil <= VNil = True
  (a :+ as) <= (b :+ bs)
    | a < b = True
    | a > b = False
    | otherwise = as <= bs

-- pure generates a vector filled with the given value of the required length
-- application uses my LinearData class to easily apply functions
instance KnownNat n => Applicative (Vector n) where
  pure a = generateVec (const a)
  (<*>) = zipWith ($)

instance KnownNat n => Monad (Vector n) where
  (>>=) =
    case natSing @n of
      1 -> \VNil f -> f a
      _ -> \(a :+ as) f -> vecHead (f a) :+ (as >>= (vecTail . f))

-- useful type class, may as well define it
instance (Num a, KnownNat n) => AdditiveGroup (Vector n a) where
  zeroV = pure 0
  negateV = fmap negate
  (^+^) = zipWith (+)

-- for scalar multiplication on vectors
instance (Num a, KnownNat n) => VectorSpace (Vector n a) where
  type Scalar (Vector n a) = a
  a *^ b = fmap (a *) b

-- find the dot product between vectors
(<.>) :: Num a => Vector n a -> Vector n a -> a
(<.>) v1 v2 = sum $ v1 ^*^ v2

instance (AdditiveGroup a, Num a, KnownNat n) => InnerSpace (Vector n a) where
  (<.>) = (Vector.<.>)

-- may as well define semigroup
instance (Semigroup a) => Semigroup (Vector n a) where
  (<>) = zipWith (<>)

-- and may as well define monoid
instance (Monoid a, KnownNat n) => Monoid (Vector n a) where
  mempty = pure mempty

-- given a functions that takes Nats and returns `a`s, create a vector
-- of the required length where each element of the vector is created
-- from the given function
generateVec :: forall a n. KnownNat n => (Fin n -> a) -> Vector n a
generateVec f = case natSing @n of
  1 -> VecSing (f 0)
  _ -> f 0 :+ generateVec (f . (+ 1))

vecReplaceElems :: [a] -> Vector n a -> Vector n a
vecReplaceElems [] v = v
vecReplaceElems (a : _) VNil = VecSing a
vecReplaceElems (a : as) (_ :+ vs) = a :+ vecReplaceElems as vs

vecFromListWithDefault :: (KnownNat n) => a -> [a] -> Vector n a
vecFromListWithDefault a as = vecReplaceElems as (generateVec (const a))

-- get the size of a given vector as a number
size :: (KnownNat n, Num b) => Vector n a -> b
size = finSize . sizeAsFin

-- given a vector, reverses it. Pretty inefficient, but this
-- a weird thing to do anyway
reverse :: Vector n a -> Vector n a
reverse VNil = VNil
reverse (a :+ as) = appendVal a (reverse as)

-- for unified construction
singleton :: a -> Vector 1 a
singleton = VecSing

-- adds an item to the end of a vector
appendVal :: a -> Vector n a -> Vector (1 + n) a
appendVal a VNil = b :+ singleton a
appendVal a (b :+ bs) = b :+ appendVal a bs

-- adds a vector to the end of a vector
(+++) :: Vector n a -> Vector m a -> Vector (n + m) a
VNil +++ vs = vs
vs +++ VNil = VNil
(a :+ rest) +++ vs = a :+ rest +++ vs

-- get the head of a vector
vecHead :: Vector (n + 1) a -> a
vecHead (a :+ _) = a

-- get the tail of the vector
vecTail :: Vector (1 + n) a -> Vector n a
vecTail (_ :+ vs) = vs

-- get the head and tail of a vector
vecSplit :: Vector (1 + n) a -> (a, Vector n a)
vecSplit v = (vecHead v, vecTail v)

-- split a vector into two components of any size (at least size one)
vecSplit' :: forall n m a. KnownNat n => Vector (n + m) a -> (Vector n a, Vector m a)
vecSplit' v = case natSing @n of
  1 -> firstCase v
  _ -> secondCase v
  where
    firstCase v' = (VecSing a, vs)
      where
        (a, vs) = vecSplit v'
    secondCase :: forall i j b. KnownNat i => Vector ((1 + i) + j) b -> (Vector (1 + i) b, Vector j b)
    secondCase (a :+ vs) = (a :+ vn, vm)
      where
        (vn, vm) = vecSplit' vs

-- drop a particular index of a vector of more than one length
dropIndex :: Fin (1 + n) -> Vector (1 + n) a -> Vector n a
dropIndex FZero (_ :+ vs) = vs
dropIndex fin (b :+ vs@(_ :+ _)) = b :+ dropIndex (reduceFin fin) vs

-- replace a given item of the vector with the input item
-- replace :: Fin n -> a -> Vector n a -> Vector n a
-- replace 0 a (VecSing _) = VecSing a
-- replace 0 a (_ :+ vs) = a :+ vs
-- replace 1 a (b :+ (VecSing _)) = b :+ VecSing a
-- replace i a (b :+ vs) = b :+ replace (i - 1) a vs

-- get the item at a given index
index :: Fin n -> Vector n a -> a
index 0 vs = vecHead vs
index i (_ :+ vs) = index (i - 1) vs

-- zip two vectors together with the index of the item zipping with
-- zipWithFin :: (Fin n -> a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
-- zipWithFin f (VecSing a) (VecSing b) = VecSing (f FZero a b)
-- zipWithFin f (a :+ as) (b :+ bs) = f FZero a b :+ zipWithFin (f . FSucc) as bs

-- map over a given vector with the index
mapWithFin :: (Fin n -> a -> b) -> Vector n a -> Vector n b
mapWithFin f v = zipWithFin (\fin a _ -> f fin a) v v

-- given a finite number, a function that gets a boolean from two finite
-- numbers, two functions a->b, a second finite number, and an a,
-- calculate b. If the boolean function applied to the finite numbers
-- is true, then use the first function. else, use the second
applyWhen ::
  Fin n ->
  (Fin n -> Fin n -> Bool) ->
  (a -> b) ->
  (a -> b) ->
  Fin n ->
  a ->
  b
applyWhen threshold comp f f' val
  | val `comp` threshold = f
  | otherwise = f'

-- apply a function to every element of the vector except the first,
--  then call this function again on the rest of the vector
applyToRest :: (a -> a) -> Vector n a -> Vector n a
applyToRest _ VNil = VNil
applyToRest f (a :+ as) = a :+ applyToRest f (fmap f as)

-- find the cross product between two three dimensional vectors
-- cheats by just applying the addition as opposed to any other thing
-- crossProd :: Num a => Vector 3 a -> Vector 3 a -> Vector 3 a
-- crossProd (ax :+ (ay :+ (VecSing az))) (bx :+ (by :+ (VecSing bz))) =
--   (ay * bz - az * by) :+ ((az * bx - ax * bz) :+ VecSing (ax * by - ay * bx))

-- -- transpose nested vectors
-- -- effectively doing matrix stuff, but it's useful elsewhere
-- vTranspose :: Vector n (Vector m a) -> Vector m (Vector n a)
-- vTranspose (VecSing a) = fmap singleton a
-- vTranspose vs@((VecSing _) :+ _) = singleton $ fmap vecHead vs
-- vTranspose (v@(_ :+ _) :+ vs) = zipWith (:+) v $ topRow :+ tails
--   where
--     tails = vTranspose $ fmap vecTail vs
--     topRow = fmap vecHead vs

extendVector :: Vector n (Vector m a) -> Vector (n GHC.TypeNats.* m) a
extendVector VNil = VNil
extendVector (v :+ vs) = v +++ extendVector vs

splitVec :: forall n m o a. Vector (n GHC.TypeNats.* m) a -> Vector n (Vector m a)
splitVec v = case natSing @n of
  1 -> VecSing v
  _ -> secondCase v
  where
    secondCase :: forall i j b. (KnownNat i, KnownNat j) => Vector ((1 + i) GHC.TypeNats.* j) b -> Vector (1 + i) (Vector j b)
    secondCase v' = frst :+ recr
      where
        (frst, rst) = vecSplit' v' :: (Vector j b, Vector (i GHC.TypeNats.* j) b)
        recr = splitVec rst

-- everyOther :: forall n a. (KnownNat n) => Vector (Mul Two n) a -> Vector Two (Vector n a)
-- everyOther v = case natSing @n of
--   OneS -> firstCase v
--   SuccS _ -> secondCase v
--   where
--     firstCase :: Vector Two a -> Vector Two (Vector One a)
--     firstCase (a :+ (VecSing b)) = singleton a :+ singleton (singleton b)
--     secondCase :: forall m b. (KnownNat ('Succ m), KnownNat m, Add Two (Mul Two m) ~ Mul Two ('Succ m)) => Vector (Mul Two ('Succ m)) b -> Vector Two (Vector ('Succ m) b)
--     secondCase (a :+ b :+ vs) = (a :+ frst) :+ VecSing (b :+ scnd)
--       where
--         (frst :+ VecSing scnd) = everyOther (vs :: Vector (Mul Two m) b)

-- halfSliceVec :: forall n a. (KnownNat n) => Fin n -> Vector (Mul Two n) a -> Vector Two (Vector n a)
-- halfSliceVec = undefined

-- func :: NatS i -> Vector (Mul Two (Mul m i)) a -> Vector Two (Vector (Mul m i) a)
-- func = undefined

-- halfSliceVec' :: forall n a i m. NatS i -> Vector (Mul Two (Mul m i)) a -> Vector Two (Vector n a)
-- halfSliceVec' s v = undefined
--   where (frst :: Vector i a, frstrst) = vecSplit' v
