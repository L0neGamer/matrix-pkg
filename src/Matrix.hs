{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall                       #-}

module Matrix where

import           Control.Applicative
import           Data.AdditiveGroup
import           Data.Foldable       (toList, find, foldl')
import           Data.VectorSpace
import           Lib
import           Vector
import qualified Data.Set as S

-- thanks to https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
-- and to https://github.com/janschultecom/idris-examples for more complex examples
newtype Matrix n m a =
  Mat (Vector n (Vector m a))
  deriving (Eq, Ord, Show)

instance Functor (Matrix n m) where
  fmap f (Mat v) = Mat $ fmap (fmap f) v

instance Foldable (Matrix n m) where
  foldr f z (Mat (VecSing vs)) = foldr f z vs
  foldr f z (Mat (v :+ vs))    = (foldr f (foldr f z (Mat vs)) v)

instance (KnownNat n, KnownNat m) => Applicative (Matrix n m) where
  pure a = Mat $ pure (pure a)
  (<*>) =
    case natSing @n of
      OneS ->
        \(Mat (VecSing fs)) (Mat (VecSing as)) -> Mat (VecSing (fs <*> as))
      SuccS _ ->
        \(Mat (fs :+ fss)) (Mat (as :+ ass)) ->
          (fs <*> as) >: ((Mat fss) <*> (Mat ass))

instance (Num a, KnownNat n, KnownNat m) => AdditiveGroup (Matrix n m a) where
  zeroV = pure 0
  negateV = fmap negate
  (^+^) = liftA2 (+)

instance (Num a, KnownNat n, KnownNat m) => VectorSpace (Matrix n m a) where
  type Scalar (Matrix n m a) = a
  a *^ b = fmap (a *) b

instance (Semigroup a) => Semigroup (Matrix n m a) where
  (<>) = Lib.zipWith (<>)

instance (Monoid a, KnownNat n, KnownNat m) => Monoid (Matrix n m a) where
  mempty = pure mempty

instance LinearData (Matrix n m) where
  (^*^) = Lib.zipWith (*)
  zipWith f (Mat vs) (Mat vs') = Mat $ Lib.zipWith (Lib.zipWith f) vs vs'

toLists :: Matrix n m a -> [[a]]
toLists (Mat v) = toList $ fmap toList v

getVec :: Matrix n m a -> Vector n (Vector m a)
getVec (Mat v) = v

(>:) :: Vector m a -> Matrix n m a -> Matrix ('Succ n) m a
(>:) = prependRow

sizeAsFin :: (KnownNat n, KnownNat m) => Matrix n m a -> (Fin n, Fin m)
sizeAsFin _ = (maxBound, maxBound)

size :: (KnownNat n, KnownNat m) => Matrix n m a -> (Integer, Integer)
size mat = (finSize fst', finSize snd')
  where
    (fst', snd') = Matrix.sizeAsFin mat

generateMat ::
     forall a n m. (KnownNat n, KnownNat m)
  => (Fin n -> Fin m -> a)
  -> Matrix n m a
generateMat f =
  case natSing @n of
    OneS    -> Mat $ VecSing (generate (f FZero))
    SuccS _ -> generate (f FZero) >: generateMat (f . FSucc)

consFrom :: (a -> b -> c) -> Vector n a -> Vector m b -> Matrix n m c
consFrom f (VecSing a) bs = (Mat . singleton . fmap (f a)) bs
consFrom f (a :+ as) bs   = fmap (f a) bs >: consFrom f as bs

identity :: (Num a, KnownNat n) => Matrix n n a
identity = generateMat (\a b -> fromIntegral $ fromEnum (a == b))

-- show a matrix (with some prettifying)
showMatrix :: Show a => Matrix n m a -> String
showMatrix m = '[' : concat items'' ++ "\n]"
  where
    items = toList $ toList <$> (getVec . fmap show) m
    maxSize = maximum $ maximum $ map (map length) items
    items' =
      map
        (("\n [" ++) .
         init .
         init .
         foldr (\a b -> padList ' ' (maxSize - length a) a ++ ", " ++ b) "")
        items
    items'' = map (++ "],") (init items') ++ [last items' ++ "]"]

-- convenience function for printing a matrix
printMatrix :: Show a => Matrix n m a -> IO ()
printMatrix = putStrLn . showMatrix

-- below are manipulation of matrices
appendRow :: Vector m a -> Matrix n m a -> Matrix ('Succ n) m a
appendRow v (Mat vs) = Mat $ appendVal v vs

prependRow :: Vector m a -> Matrix n m a -> Matrix ('Succ n) m a
prependRow v (Mat vs) = Mat $ v :+ vs

appendCol :: Vector n a -> Matrix n m a -> Matrix n ('Succ m) a
appendCol v (Mat vs) = Mat $ Lib.zipWith appendVal v vs

prependCol :: Vector n a -> Matrix n m a -> Matrix n ('Succ m) a
prependCol v (Mat vs) = Mat $ Lib.zipWith (:+) v vs

concatCols :: Matrix n m a -> Matrix n o a -> Matrix n (Add m o) a
concatCols (Mat vs) (Mat vs') = Mat $ Lib.zipWith (+++) vs vs'

concatRows :: Matrix n m a -> Matrix o m a -> Matrix (Add n o) m a
concatRows (Mat vs) (Mat vs') = Mat $ vs +++ vs'

dropCol :: Fin ('Succ m) -> Matrix n ('Succ m) b -> Matrix n m b
dropCol a (Mat vs) = Mat $ fmap (dropIndex a) vs

dropRow :: Fin ('Succ n) -> Matrix ('Succ n) m b -> Matrix n m b
dropRow a (Mat vs) = Mat $ dropIndex a vs

setAtMatrix :: Fin n -> Fin m -> a -> Matrix n m a -> Matrix n m a
setAtMatrix i j a (Mat vs) = Mat $ replace i (replace j a (index i vs)) vs

getAtMatrix :: Fin n -> Fin m -> Matrix n m a -> a
getAtMatrix i j (Mat vs) = index j $ index i vs

subMatrix ::
     Fin ('Succ n)
  -> Fin ('Succ m)
  -> Matrix ('Succ n) ('Succ m) b
  -> Matrix n m b
subMatrix i j = dropCol j . dropRow i

trace :: (Num a) => Matrix n n a -> a
trace (Mat (VecSing (VecSing a))) = a
trace m@(Mat ((a :+ _) :+ _))     = a + trace (subMatrix FZero FZero m)

-- below are operations on matrices
-- transpose a nxm matrix to an mxn matrix
transpose :: Matrix n m a -> Matrix m n a
transpose (Mat (VecSing a)) = Mat $ fmap singleton a
transpose (Mat vs@((VecSing _) :+ _)) = Mat $ singleton $ fmap vecHead vs
transpose (Mat (v@(_ :+ _) :+ vs)) = prependCol v $ topRow >: tails
  where
    tails = transpose $ Mat $ fmap vecTail vs
    topRow = fmap vecHead vs

subtractRow :: forall a n m. (KnownNat n, KnownNat m, Num a)
  => Matrix n m a -> Fin n -> Fin m -> Fin n -> Matrix n m a
subtractRow mat row col currRow = mat'
  where selectedCols = finFrom col
        multVal = getAtMatrix currRow col mat
        values = map (\col' -> (col',getAtMatrix currRow col' mat - (multVal * getAtMatrix row col' mat))) selectedCols
        mat' = foldl' (\newMat (col', val) -> setAtMatrix currRow col' val newMat) mat values

-- compared to original code, this doesn't take into account errors with floating point numbers
-- as such, be careful
calcFromElem :: forall a n m. (KnownNat n, KnownNat m, Fractional a) =>
  Matrix n m a -> Fin n -> Fin m -> Matrix n m a
calcFromElem mat row col = mat'
  where selectedCols = finFrom col
        selectedValue = getAtMatrix row col mat
        values = map (\col' -> (col', getAtMatrix row col' mat / selectedValue)) selectedCols
        multipliedDownMatrix = foldr (\(col', val) newMat -> setAtMatrix row col' val newMat) mat values
        mat' = foldl' (\newMat row' -> subtractRow newMat row col row') multipliedDownMatrix (filter (/= row) fins)

onCol :: forall a n m. (KnownNat n, KnownNat m, Fractional a, Eq a) =>
  Matrix n m a -> Fin m -> S.Set (Fin n) -> Maybe (Matrix n m a, Fin n)
onCol mat col previousRows = selectedRow >>= \row -> Just (calcFromElem mat row col, row)
  where selectedRow = find (\row -> (not $ elem row previousRows) && (getAtMatrix row col mat /= 0)) fins

rank' :: Maybe (Matrix n m a, Fin n) -> Matrix n m a -> S.Set (Fin n) -> (Matrix n m a, S.Set (Fin n))
rank' Nothing mat set = (mat, set)
rank' (Just (mat, row)) _ set = (mat, S.insert row set)

-- if you can find a way to check that n==m, and that the determinant of the matrix
--  is non zero, you can take a shortcut for square matrices
-- | matRows == matCols && det mat /= 0 = maxRank
rank ::
     forall a n m. (KnownNat n, KnownNat m, Fractional a, Eq a)
  => Matrix n m a
  -> Integer
rank mat = fromIntegral $ length setToCheck
  where cols = fins :: [Fin m]
        (_, setToCheck) = foldl' (\(mat', set) col -> rank' (onCol mat' col set) mat' set) (mat, S.empty) cols

-- help from: https://github.com/janschultecom/idris-examples/blob/master/matrixmult.idr#L21
-- helper function to multiply a vector over a matrix
multVectMat :: Num a => Vector m a -> Matrix n m a -> Vector n a
multVectMat xs (Mat (VecSing v)) = singleton $ dotProd xs v
multVectMat xs (Mat (v :+ vs))   = dotProd xs v :+ multVectMat xs (Mat vs)

-- multiply two matrices together
multiplyMat :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
multiplyMat (Mat (VecSing vs)) b =
  Mat $ (singleton . multVectMat vs . transpose) b
multiplyMat (Mat (v :+ vs)) b =
  multVectMat v (transpose b) >: multiplyMat (Mat vs) b

checkerboard :: Num a => Matrix n m a -> Matrix n m a
checkerboard (Mat vs) =
  Mat $ fmap (applyToRest negate) $ applyToRest (fmap negate) vs

matrixOfMinors ::
     forall a n. (Num a, KnownNat n)
  => Matrix n n a
  -> Matrix n n a
matrixOfMinors m =
  case natSing @n of
    OneS    -> Mat $ (VecSing . VecSing . det) m
    SuccS _ -> fmap det $ generateMat (\i j -> subMatrix i j m)

-- -- thanks to https://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html
inverseMatrix ::
     forall a n. (Fractional a, Eq a, KnownNat n)
  => Matrix n n a
  -> Maybe (Matrix n n a)
inverseMatrix m
  | determinant == 0 = Nothing
  | otherwise = Just $ transpose $ fmap (/ determinant) (cboardThenMOM m)
  where
    determinant = det m
    cboardThenMOM = checkerboard . matrixOfMinors

det ::
     forall a n. (Num a, KnownNat n)
  => Matrix n n a
  -> a
det m =
  case natSing @n of
    OneS -> (vecHead . vecHead . getVec) m
    SuccS _ ->
      sum . vecHead . getVec $
      Lib.zipWith (*) m $ (checkerboard . matrixOfMinors) m

innerProduct :: Num a => Matrix n n a -> Matrix n n a -> Matrix n n a
innerProduct m1 m2 = m1 *.* transpose m2

-- above two lines are virtually identical, just to make compiler happy
-- below are some convienience binary operators for matrices
(*.*) :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
(*.*) = multiplyMat
