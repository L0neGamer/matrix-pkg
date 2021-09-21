module Matrix where

import Control.Applicative (Applicative (liftA2))
import Data.AdditiveGroup (AdditiveGroup (negateV, zeroV, (^+^)))
import Data.Foldable (find, foldl', toList)
import qualified Data.Set as S
import Data.VectorSpace (VectorSpace (..))
import Lib
import Vector
import Prelude hiding (zipWith)

-- thanks to https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
-- and to https://github.com/janschultecom/idris-examples for more complex examples
newtype Matrix n m a
  = Mat (Vector n (Vector m a))
  deriving (Eq, Ord, Show)

-- additional types for vertical and horizontal matrix vectors
type VVec n a = Matrix n One a

type HVec n a = Matrix One n a

-- define the LinearData type
instance LinearData (Matrix n m) where
  zipWith f (Mat vs) (Mat vs') = Mat $ zipWith (zipWith f) vs vs'

instance Functor (Matrix n m) where
  fmap f (Mat v) = Mat $ fmap (fmap f) v

-- fold over the matrix row by row
instance Foldable (Matrix n m) where
  foldr f z (Mat (VecSing vs)) = foldr f z vs
  foldr f z (Mat (v :+ vs)) = foldr f (foldr f z (Mat vs)) v

-- pure it just an appropriately sized matrix filled with
instance (KnownNat n, KnownNat m) => Applicative (Matrix n m) where
  pure a = Mat $ pure (pure a)
  (<*>) = zipWith ($)

-- useful type class, may as well define it
instance (Num a, KnownNat n, KnownNat m) => AdditiveGroup (Matrix n m a) where
  zeroV = pure 0
  negateV = fmap negate
  (^+^) = liftA2 (+)

-- for scalar multiplication on matrices
instance (Num a, KnownNat n, KnownNat m) => VectorSpace (Matrix n m a) where
  type Scalar (Matrix n m a) = a
  a *^ b = fmap (a *) b

-- may as well implement monoid and semigroup again in the same way
instance (Semigroup a) => Semigroup (Matrix n m a) where
  (<>) = zipWith (<>)

instance (Monoid a, KnownNat n, KnownNat m) => Monoid (Matrix n m a) where
  mempty = pure mempty

showVVec :: Show a => VVec n a -> String
showVVec v = "[\n" ++ foldr (\a s -> "    " ++ show a ++ "\n" ++ s) "]" v

showHVec :: Show a => HVec n a -> String
showHVec v =
  "[\n" ++ init (foldr (\a s -> "    " ++ show a ++ "," ++ s) "" v) ++ "\n]"

-- convert to lists for easy access, in theory
toLists :: Matrix n m a -> [[a]]
toLists (Mat v) = toList $ fmap toList v

-- get the vector that makes up the matrix
getVec :: Matrix n m a -> Vector n (Vector m a)
getVec (Mat v) = v

-- get the single value in a matrix
getVal :: Matrix One One a -> a
getVal (Mat (VecSing (VecSing a))) = a

-- slap a vector onto the top of the matrix
-- full definition later
(>:) :: Vector m a -> Matrix n m a -> Matrix ('Succ n) m a
(>:) = prependRow

-- get the size of the dimensions of the matrix in Fins
sizeAsFin :: (KnownNat n, KnownNat m) => Matrix n m a -> (Fin n, Fin m)
sizeAsFin _ = (maxBound, maxBound)

-- get size of the matrix in terms of any number
size :: (KnownNat n, KnownNat m, Num a) => Matrix n m b -> (a, a)
size mat = (finSize fst', finSize snd')
  where
    (fst', snd') = Matrix.sizeAsFin mat

-- generate a matrix from a given function that takes two Fins
generateMat ::
  forall a n m.
  (KnownNat n, KnownNat m) =>
  (Fin n -> Fin m -> a) ->
  Matrix n m a
generateMat f = case natSing @n of
  OneS -> Mat $ VecSing (generateVec (f FZero))
  SuccS _ -> generateVec (f FZero) >: generateMat (f . FSucc)

-- construct a matrix from a function taking two vars and two vectors
consFrom :: (a -> b -> c) -> Vector n a -> Vector m b -> Matrix n m c
consFrom f (VecSing a) bs = (Mat . singleton . fmap (f a)) bs
consFrom f (a :+ as) bs = fmap (f a) bs >: consFrom f as bs

matReplaceElems :: [[a]] -> Matrix n m a -> Matrix n m a
matReplaceElems [] m = m
matReplaceElems (a : _) (Mat (VecSing v)) = Mat $ VecSing (vecReplaceElems a v)
matReplaceElems (a : as) (Mat (v :+ vs)) = vecReplaceElems a v >: matReplaceElems as (Mat vs)

matFromListWithDefault :: (KnownNat n, KnownNat m) => a -> [[a]] -> Matrix n m a
matFromListWithDefault a as = matReplaceElems as (generateMat (\_ _ -> a))

numMatFromList :: (KnownNat n, KnownNat m, Num a) => [[a]] -> Matrix n m a
numMatFromList = matFromListWithDefault 0

-- create the identity matrix
identity :: (Num a, KnownNat n) => Matrix n n a
identity = generateMat (\a b -> fromIntegral $ fromEnum (a == b))

-- show a matrix (with some prettifying)
showMatrix :: Show a => Matrix n m a -> String
showMatrix m@(Mat (VecSing _)) = showHVec m
showMatrix m@(Mat ((VecSing _) :+ _)) = showVVec m
showMatrix m = '[' : concat items'' ++ "\n]"
  where
    showedM@(Mat vs) = fmap show m
    maxCols = fmap (maximum . fmap length) $ getVec $ Matrix.transpose showedM
    showedM' =
      toList $
        fmap
          ( toList
              . mapWithFin
                (\fin str -> padList ' ' (index fin maxCols - length str) str ++ ", ")
          )
          vs
    items' = map (("\n [" ++) . init . init . concat) showedM'
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

getRow :: Fin n -> Matrix n m a -> Vector m a
getRow i (Mat v) = index i v

getCol :: Fin m -> Matrix n m a -> Vector n a
getCol i (Mat v) = fmap (index i) v

setRow :: Fin n -> Vector m a -> Matrix n m a -> Matrix n m a
setRow i row (Mat v) = Mat (replace i row v)

setCol :: Fin m -> Vector n a -> Matrix n m a -> Matrix n m a
setCol i (VecSing a) (Mat (VecSing v)) = Mat (VecSing (replace i a v))
setCol i (a :+ as) (Mat (v :+ vs)) =
  prependRow (replace i a v) (setCol i as (Mat vs))

-- gets rid of one row and column from a matrix
subMatrix ::
  Fin ('Succ n) ->
  Fin ('Succ m) ->
  Matrix ('Succ n) ('Succ m) b ->
  Matrix n m b
subMatrix i j = dropCol j . dropRow i

-- get the trace of a matrix
trace :: (Num a) => Matrix n n a -> a
trace (Mat (VecSing (VecSing a))) = a
trace m@(Mat ((a :+ _) :+ _)) = a + trace (subMatrix FZero FZero m)

-- below are operations on matrices
-- transpose a nxm matrix to an mxn matrix
transpose :: Matrix n m a -> Matrix m n a
transpose = Mat . Vector.transpose . getVec

-- TODO: work out what this does
-- compared to original code, this doesn't take into account errors with floating point numbers
-- as such, be careful
calcFromElem ::
  forall a n m.
  (KnownNat n, KnownNat m, Fractional a) =>
  Matrix n m a ->
  Fin n ->
  Fin m ->
  Matrix n m a
calcFromElem mat row col =
  Mat $
    mapWithFin (applyWhen row (/=) subtractFromRow id) vs
  where
    selectedValue = getAtMatrix row col mat
    valuesVec = fmap (/ selectedValue) (getRow row mat)
    mat'@(Mat vs) = setRow row valuesVec mat
    subtractFromRow currRow =
      zipWithFin
        (applyWhen col (>=) (-) const)
        currRow
        (fmap (* index col currRow) (getRow row mat'))

onCol ::
  forall a n m.
  (KnownNat n, KnownNat m, Fractional a, Eq a) =>
  Matrix n m a ->
  Fin m ->
  S.Set (Fin n) ->
  Maybe (Matrix n m a, Fin n)
onCol mat col previousRows =
  selectedRow
    >>= \row -> Just (calcFromElem mat row col, row)
  where
    colVec = getCol col mat
    selectedRow =
      find (\row -> row `S.notMember` previousRows && index row colVec /= 0) fins

-- if you can find a way to check that n==m, and that the determinant of the matrix
--  is non zero, you can take a shortcut for square matrices

-- | matRows == matCols && det mat /= 0 = maxRank
rank' ::
  forall a n m.
  (KnownNat n, KnownNat m, Fractional a, Eq a) =>
  Matrix n m a ->
  (Matrix n m a, S.Set (Fin n))
rank' mat = foldl' f (mat, S.empty) fins
  where
    f (mat', set) col =
      maybe
        (mat', set)
        (\(mat'', row) -> (mat'', S.insert row set))
        (onCol mat' col set)

-- finds how many linearly independent columns there are
rank ::
  forall a n m.
  (KnownNat n, KnownNat m, Fractional a, Eq a) =>
  Matrix n m a ->
  Integer
rank = fromIntegral . length . snd . rank'

-- help from: https://github.com/janschultecom/idris-examples/blob/master/matrixmult.idr#L21
-- helper function to multiply a vector over a matrix
multVectMat :: (Num a) => Vector m a -> Matrix n m a -> Vector n a
multVectMat xs (Mat (VecSing v)) = singleton $ xs <.> v
multVectMat xs (Mat (v :+ vs)) = xs <.> v :+ multVectMat xs (Mat vs)

-- multiply two matrices together
multiplyMat :: (Num a) => Matrix n m a -> Matrix m o a -> Matrix n o a
multiplyMat (Mat (VecSing vs)) b =
  Mat $ (singleton . multVectMat vs . Matrix.transpose) b
multiplyMat (Mat (v :+ vs)) b =
  multVectMat v (Matrix.transpose b) >: multiplyMat (Mat vs) b

-- creates a checkerboard of negatives and positives
checkerboard :: Num a => Matrix n m a -> Matrix n m a
checkerboard (Mat vs) =
  Mat $ applyToRest negate <$> applyToRest (fmap negate) vs

-- generate a matrix where each element is the determinant
-- of the submatrices not including that row and column
matrixOfMinors ::
  forall a n. (Num a, KnownNat n) => Matrix n n a -> Matrix n n a
matrixOfMinors m = case natSing @n of
  OneS -> Mat $ (VecSing . VecSing . det) m
  SuccS _ -> det <$> generateMat (\i j -> subMatrix i j m)

-- -- thanks to https://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html
inverseMatrix ::
  forall a n.
  (Fractional a, Eq a, KnownNat n) =>
  Matrix n n a ->
  Maybe (Matrix n n a)
inverseMatrix m
  | determinant == 0 = Nothing
  | otherwise = Just $ Matrix.transpose $ fmap (/ determinant) (cboardThenMOM m)
  where
    determinant = det m
    cboardThenMOM = checkerboard . matrixOfMinors

-- find the determinant of a square matrix
det :: forall a n. (Num a, KnownNat n) => Matrix n n a -> a
det m = case natSing @n of
  OneS -> getVal m
  SuccS _ -> sum . vecHead . getVec $ m ^*^ (checkerboard . matrixOfMinors) m

-- find the inner product between two matrices
innerProduct :: Num a => Matrix n n a -> Matrix n n a -> Matrix n n a
innerProduct m1 m2 = m1 *.* Matrix.transpose m2

-- above two lines are virtually identical, just to make compiler happy
-- below are some convienience binary operators for matrices
(*.*) :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
(*.*) = multiplyMat

infixl 7 *.*

-- find the dot product between two vector matrices
dotProd :: Num a => Matrix n One a -> Matrix n One a -> a
dotProd m n = getCol FZero m <.> getCol FZero n

-- given a vector of matrices, stick them together as if they were
-- horizontal, ie column wise
concatMatricesCol :: Vector n (Matrix i j a) -> Matrix i (Mul n j) a
concatMatricesCol (VecSing m) = m
concatMatricesCol (m :+ ms) = m `concatCols` concatMatricesCol ms

-- given a vector of matrices, stick them together as if they were
-- vertical, ie row wise
concatMatricesRow :: Vector n (Matrix i j a) -> Matrix (Mul n i) j a
concatMatricesRow (VecSing m) = m
concatMatricesRow (m :+ ms) = m `concatRows` concatMatricesRow ms

-- given a matrix filled with matrices, flatten it
expandNested :: Matrix n m (Matrix i j a) -> Matrix (Mul n i) (Mul m j) a
expandNested (Mat v) = concatMatricesRow $ fmap concatMatricesCol v

toVVec :: Vector n a -> VVec n a
toVVec = Mat . fmap VecSing
