module Vector where

import           Control.Applicative
import           Data.AdditiveGroup
import           Data.VectorSpace
import           Lib
import           Prelude             hiding (zipWith, reverse)

-- below is a useful link for vector stuff
-- https://stackoverflow.com/questions/62039392/how-do-i-allow-one-constraint-to-imply-another-in-haskell/62040229#62040229

-- create the vector data type
-- the prepend operator increases the vector size by one
-- the minimum vector size is one
data Vector (n :: Nat) a where
  VecSing :: a -> Vector One a
  (:+) :: a -> Vector n a -> Vector ('Succ n) a
infixr 8 :+

deriving instance Show a => Show (Vector n a)

-- we should definitely define LinearData for Vectors as it's very useful
instance LinearData (Vector n) where
  zipWith f (VecSing a) (VecSing b) = VecSing (f a b)
  zipWith f (a :+ as) (b :+ bs)     = f a b :+ zipWith f as bs

instance Functor (Vector n) where
  fmap f (VecSing a) = VecSing (f a)
  fmap f (a :+ vs)   = f a :+ fmap f vs

instance Foldable (Vector n) where
  foldr f z (VecSing a) = a `f` z
  foldr f z (a :+ vs)   = a `f` foldr f z vs

instance Eq a => Eq (Vector n a) where
  v1 == v2 = and $ zipWith (==) v1 v2

instance Traversable (Vector n) where
  traverse f (VecSing a) = VecSing <$> f a
  traverse f (a :+ vs)   = (:+) <$> f a <*> traverse f vs

-- we can say that ordering is a thing we can do
instance Ord a => Ord (Vector n a) where
  (VecSing a) <= (VecSing b) = a <= b
  (a :+ as) <= (b :+ bs)
    | a < b = True
    | a > b = False
    | otherwise = as <= bs

-- pure generates a vector filled with the given value of the required length
-- application uses my LinearData class to easily apply functions
instance KnownNat n => Applicative (Vector n) where
  pure a = generate (\_ -> a)
  (<*>) = zipWith ($)

instance KnownNat n => Monad (Vector n) where
  (>>=) =
    case natSing @n of
      OneS    -> \(VecSing a) f -> f a
      SuccS _ -> \(a :+ as) f -> (vecHead $ f a) :+ (as >>= (vecTail . f))

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

-- given a functions that takes Fins and returns `a`s, create a vector
-- of the required length where each element of the vector is created
-- from the given function
generate :: forall a n . KnownNat n => (Fin n -> a) -> Vector n a
generate f = case natSing @n of
  OneS    -> VecSing (f FZero)
  SuccS _ -> f FZero :+ generate (f . FSucc)

vecReplaceElems :: [a] -> Vector n a -> Vector n a
vecReplaceElems []     v           = v
vecReplaceElems (a:_ ) (VecSing _) = VecSing a
vecReplaceElems (a:as) (_:+vs    ) = a :+ vecReplaceElems as vs

vecFromListWithDefault :: (KnownNat n) => a -> [a] -> Vector n a
vecFromListWithDefault a as = vecReplaceElems as (generate (\_ -> a))

-- get the size of a given vector as a Fin
sizeAsFin :: KnownNat n => Vector n a -> Fin n
sizeAsFin _ = maxBound

-- get the size of a given vector as a number
size :: (KnownNat n, Num b) => Vector n a -> b
size = finSize . sizeAsFin

-- given a vector, reverses it. Pretty inefficient, but this 
-- a weird thing to do anyway
reverse :: Vector n a -> Vector n a
reverse v@(VecSing _) = v
reverse (  a:+as    ) = appendVal a (reverse as)

-- for unified construction
singleton :: a -> Vector One a
singleton = VecSing

-- adds an item to the end of a vector
appendVal :: a -> Vector n a -> Vector ( 'Succ n) a
appendVal a (VecSing b) = b :+ singleton a
appendVal a (b:+bs    ) = b :+ appendVal a bs

-- adds a vector to the end of a vector
(+++) :: Vector n a -> Vector m a -> Vector (Add n m) a
(VecSing a) +++ vs = a :+ vs
(a:+rest  ) +++ vs = a :+ rest +++ vs

-- get the head of a vector
vecHead :: Vector n a -> a
vecHead (VecSing a) = a
vecHead (a:+_     ) = a

-- get the tail of the vector
-- only works if the vector is more than one length
vecTail :: Vector ( 'Succ n) a -> Vector n a
vecTail (_:+vs) = vs

-- get the head and tail of a vector
vecSplit :: Vector ( 'Succ n) a -> (a, Vector n a)
vecSplit v = (vecHead v, vecTail v)

vecDropLast :: Vector ( 'Succ n) a -> Vector n a
vecDropLast (v:+(   VecSing _)) = VecSing v
vecDropLast (v:+vs@(_:+_     )) = v :+ vecDropLast vs

-- drop a particular index of a vector of more than one length
dropIndex :: Fin ( 'Succ n) -> Vector ( 'Succ n) a -> Vector n a
dropIndex FZero         (_:+vs            ) = vs
dropIndex (FSucc FZero) (b:+(   VecSing _)) = VecSing b
dropIndex (FSucc fin  ) (b:+vs@(_:+_     )) = b :+ dropIndex fin vs

-- replace a given item of the vector with the input item
replace :: Fin n -> a -> Vector n a -> Vector n a
replace FZero         a (VecSing _     ) = VecSing a
replace FZero         a (_:+vs         ) = a :+ vs
replace (FSucc FZero) a (b:+(VecSing _)) = b :+ VecSing a
replace (FSucc fin  ) a (b:+vs         ) = b :+ replace fin a vs

-- get the item at a given index
index :: Fin n -> Vector n a -> a
index FZero     vs      = vecHead vs
index (FSucc i) (_:+vs) = index i vs

-- zip two vectors together with the index of the item zipping with
zipWithFin :: (Fin n -> a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWithFin f (VecSing a) (VecSing b) = VecSing (f FZero a b)
zipWithFin f (a:+as) (b:+bs) = (f FZero a b) :+ zipWithFin (f . FSucc) as bs

-- map over a given vector with the index
mapWithFin :: (Fin n -> a -> b) -> Vector n a -> Vector n b
mapWithFin f v = zipWithFin (\fin a _ -> f fin a) v v

-- given a finite number, a function that gets a boolean from two finite
-- numbers, two functions a->b, a second finite number, and an a,
-- calculate b. If the boolean function applied to the finite numbers
-- is true, then use the first function. else, use the second
applyWhen
  :: Fin n
  -> (Fin n -> Fin n -> Bool)
  -> (a -> b)
  -> (a -> b)
  -> Fin n
  -> a
  -> b
applyWhen threshold comp f f' val | val `comp` threshold = f
                                  | otherwise            = f'

-- apply a function to every element of the vector except the first,
--  then call this function again on the rest of the vector
applyToRest :: (a -> a) -> Vector n a -> Vector n a
applyToRest _ (VecSing a) = VecSing a
applyToRest f (a:+as    ) = a :+ applyToRest f (fmap f as)

-- find the cross product between two three dimensional vectors
-- cheats by just applying the addition as opposed to any other thing
crossProd :: Num a => Vector Three a -> Vector Three a -> Vector Three a
crossProd (ax:+(ay:+(VecSing az))) (bx:+(by:+(VecSing bz))) =
  (ay * bz - az * by) :+ ((az * bx - ax * bz) :+ VecSing (ax * by - ay * bx))

-- transpose nested vectors
-- effectively doing matrix stuff, but it's useful elsewhere
transpose :: Vector n (Vector m a) -> Vector m (Vector n a)
transpose (   VecSing a        ) = fmap singleton a
transpose vs@((  VecSing _):+_ ) = singleton $ fmap vecHead vs
transpose (   v@(_:+_     ):+vs) = zipWith (:+) v $ topRow :+ tails
 where
  tails  = transpose $ fmap vecTail vs
  topRow = fmap vecHead vs

extendVector :: Vector n (Vector m a) -> Vector (Mul n m) a
extendVector (VecSing v) = v
extendVector (v:+vs    ) = v +++ extendVector vs
