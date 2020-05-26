{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall                       #-}

module Matrix where

import           Data.Foldable   (toList)
import           Data.Singletons
import           Lib
import           Vector

-- thanks to https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
-- and to https://github.com/janschultecom/idris-examples for more complex examples
newtype Matrix n m a =
  Mat (Vector n (Vector m a))
  deriving (Eq, Ord)

instance Functor (Matrix n m) where
  fmap f (Mat v) = Mat $ fmap (fmap f) v

instance Show a => Show (Matrix n m a) where
  show = showMatrix

toLists :: Matrix n m a -> [[a]]
toLists (Mat v) = toList $ fmap toList v

getVec :: Matrix n m a -> Vector n (Vector m a)
getVec (Mat v) = v

(>:) :: Vector m a -> Matrix n m a -> Matrix ('Succ n) m a
(>:) = prependRow

-- below are construction and displaying matrices
squareMatrix :: a -> Vector n b -> Matrix n n a
squareMatrix a v = consFrom (\_ _ -> a) v v

generateMat' :: Sing n -> Sing m -> (Fin n -> Fin m -> a) -> Matrix n m a
generateMat' SOne sm f = Mat $ VecSing (generate' sm (f FZero))
generateMat' (SSucc sn) sm f =
  generate' sm (f FZero) >: generateMat' sn sm (f . FSucc)

generateMat :: (SingI n, SingI m) => (Fin n -> Fin m -> a) -> Matrix n m a
generateMat = generateMat' sing sing

consFrom :: (a -> b -> c) -> Vector n a -> Vector m b -> Matrix n m c
consFrom f (VecSing a) bs = (Mat . singleton . fmap (f a)) bs
consFrom f (a :+ as) bs   = fmap (f a) bs >: consFrom f as bs

identity :: (Num a, SingI n) => Matrix n n a
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
appendCol v (Mat vs) = Mat $ vecZipWith appendVal v vs

prependCol :: Vector n a -> Matrix n m a -> Matrix n ('Succ m) a
prependCol v (Mat vs) = Mat $ vecZipWith (:+) v vs

concatCols :: Matrix n m a -> Matrix n o a -> Matrix n (Add m o) a
concatCols (Mat vs) (Mat vs') = Mat $ vecZipWith (+++) vs vs'

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

-- zip together two equally sized matrices
matZipWith :: (a -> b -> c) -> Matrix n m a -> Matrix n m b -> Matrix n m c
matZipWith f (Mat vs) (Mat vs') = Mat $ vecZipWith (vecZipWith f) vs vs'

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

matrixOfMinors' :: (Num a) => Sing n -> Matrix n n a -> Matrix n n a
matrixOfMinors' SOne m = Mat $ (VecSing . VecSing . det) m
matrixOfMinors' s@(SSucc s') m =
  fmap (det' s') $ generateMat' s s (\i j -> subMatrix i j m)

matrixOfMinors :: (Num a, SingI n) => Matrix n n a -> Matrix n n a
matrixOfMinors = matrixOfMinors' sing

-- -- thanks to https://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html
inverseMatrix ::
     (Fractional a, Eq a, SingI n) => Matrix n n a -> Maybe (Matrix n n a)
inverseMatrix m
  | determinant == 0 = Nothing
  | otherwise = Just $ transpose $ fmap (/ determinant) (cboardThenMOM m)
  where
    determinant = det m
    cboardThenMOM = checkerboard . matrixOfMinors

-- -- find the determinant for a square matrix
det' :: Num a => Sing n -> Matrix n n a -> a
det' SOne (Mat (VecSing (VecSing a))) = a
det' s@(SSucc _) m@(Mat (_ :+ _)) =
  sum . vecHead . getVec $ m ..* (checkerboard . matrixOfMinors' s) m

det :: (Num a, SingI n) => Matrix n n a -> a
det = det' sing

-- above two lines are virtually identical, just to make compiler happy
-- below are some convienience binary operators for matrices
(*.*) :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
(*.*) = multiplyMat

(..*) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
(..*) = matZipWith (*)

(..+) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
(..+) = matZipWith (+)
