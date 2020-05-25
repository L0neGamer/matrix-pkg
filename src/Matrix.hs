{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- module Matrix (Matrix, identity, mapMatrix, det, consFrom) where
module Matrix where

import           Data.Foldable (toList)
import           Data.Maybe    (fromJust)
import           Lib
import           Vector

-- thanks to https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
-- and to https://github.com/janschultecom/idris-examples for more complex examples
type Matrix n m a = Vector n (Vector m a)

-- below are construction and displaying matrices
squareMatrix :: a -> Vector n b -> Matrix n n a
squareMatrix a v = consFrom (\_ _ -> a) v v

consFrom :: (a -> b -> c) -> Vector n a -> Vector m b -> Matrix n m c
consFrom f (Single a) bs = singleton $ fmap (f a) bs
consFrom f (a :+ as) bs  = fmap (f a) bs :+ consFrom f as bs

consFromCoord ::
     (Integer -> Integer -> a) -> Vector n b -> Vector m c -> Matrix n m a
consFromCoord f as bs = mapMatrix (\(x, y) -> f x y) positions
  where
    (indexVecA, indexVecB) = (incrementingVec as, incrementingVec bs)
    positions = consFrom (,) indexVecA indexVecB

identity :: Num a => Vector n b -> Matrix n n a
identity vs = consFromCoord (\a b -> fromIntegral $ fromEnum (a == b)) vs vs

mapMatrix :: (a -> b) -> Matrix n m a -> Matrix n m b
mapMatrix f m = fmap (fmap f) m

-- show a matrix (with some prettifying)
showMatrix :: Show a => Matrix n m a -> String
showMatrix m = '[' : foldr (++) "\n]" items''
  where
    items = toList $ fmap toList $ mapMatrix show m
    maxSize = maximum $ maximum $ map (map length) items
    items' =
      map
        (("\n [" ++) .
         init .
         init .
         foldr (\a b -> a ++ ", " ++ b) "" .
         map (\item -> padList ' ' (maxSize - length item) item))
        items
    items'' = map (++ "],") (init items') ++ [last items' ++ "]"]

-- convenience function for printing a matrix
printMatrix :: Show a => Matrix n m a -> IO ()
printMatrix = putStrLn . showMatrix

-- below are manipulation of matrices
appendRow :: Vector m a -> Matrix n m a -> Matrix ('Succ n) m a
appendRow v m = appendVal v m

prependRow :: Vector m a -> Matrix n m a -> Matrix ('Succ n) m a
prependRow v m = v :+ m

appendCol :: Vector n a -> Matrix n m a -> Matrix n ('Succ m) a
appendCol v m = vecZipWith appendVal v m

prependCol :: Vector n a -> Matrix n m a -> Matrix n ('Succ m) a
prependCol v m = vecZipWith (:+) v m

concatCols :: Matrix n m a -> Matrix n o a -> Matrix n (Add m o) a
concatCols = vecZipWith (+++)

concatRows :: Matrix n m a -> Matrix o m a -> Matrix (Add n o) m a
concatRows = (+++)

dropCol :: Integer -> Matrix n ('Succ m) b -> Maybe (Matrix n m b)
dropCol a = sequence . fmap (dropItem a)

dropRow :: Integer -> Matrix ('Succ n) m b -> Maybe (Matrix n m b)
dropRow a = dropItem a

setAtMatrix :: Integer -> Integer -> a -> Matrix n m a -> Maybe (Matrix n m a)
setAtMatrix i j a m = getAt i m >>= setAt j a >>= \col' -> setAt i col' m

getAtMatrix :: Integer -> Integer -> Matrix n m a -> Maybe a
getAtMatrix i j m = getAt i m >>= getAt j

-- drop the ith row and the jth column, or the last of either if out of bounds
subMatrix ::
     Integer -> Integer -> Matrix ('Succ n) ('Succ m) b -> Maybe (Matrix n m b)
subMatrix i j m = dropRow i m >>= dropCol j

trace :: Num a => Matrix n n a -> a
trace (Single (Single a)) = a
trace ((a :+ _) :+ m')    = a + trace (fromJust $ dropCol 0 m')
trace _                   = error "unreachable pattern in trace"

-- below are operations on matrices
-- transpose a nxm matrix to an mxn matrix
transpose :: Matrix n m a -> Matrix m n a
transpose (Single a) = fmap singleton a
transpose m@((Single _) :+ _) = singleton $ fmap vecHead m
transpose (v@(_ :+ _) :+ vs) = vecZipWith (:+) v $ topRow :+ (transpose tails)
  where
    tails = fmap vecTail vs
    topRow = fmap vecHead vs

-- zip together two equally sized matrices
matZipWith :: (a -> b -> c) -> Matrix n m a -> Matrix n m b -> Matrix n m c
matZipWith f a b = vecZipWith (vecZipWith f) a b

-- help from: https://github.com/janschultecom/idris-examples/blob/master/matrixmult.idr#L21
-- helper function to multiply a vector over a matrix
multVectMat :: Num a => Vector m a -> Matrix n m a -> Vector n a
multVectMat xs (Single v) = singleton $ dotProd xs v
multVectMat xs (v :+ vs)  = dotProd xs v :+ multVectMat xs vs

-- multiply two matrices together
multiplyMat :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
multiplyMat (Single vs) b = (singleton . multVectMat vs . transpose) b
multiplyMat (v :+ vs) b   = multVectMat v (transpose b) :+ multiplyMat vs b

matrixOfMinors :: Num a => Matrix n n a -> Matrix n n a
matrixOfMinors m@(Single _) = m
matrixOfMinors m@(_ :+ _) =
  consFromCoord (\i j -> det $ fromJust $ subMatrix i j m) m m

checkerboard :: Num a => Matrix n m a -> Matrix n m a
checkerboard = fmap (applyToRest negate) . applyToRest (fmap negate)

cBoardThenMOM :: Num a => Matrix n n a -> Matrix n n a
cBoardThenMOM = checkerboard . matrixOfMinors

-- thanks to https://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html
inverseMatrix :: (Fractional a, Eq a) => Matrix n n a -> Maybe (Matrix n n a)
inverseMatrix (Single (Single 0)) = Nothing
inverseMatrix (Single (Single a)) = Just (Single (Single (recip a)))
inverseMatrix m
  | determinant == 0 = Nothing
  | otherwise = Just $ transpose $ mapMatrix (/ determinant) (cBoardThenMOM m)
  where
    determinant = det m

-- find the determinant for a square matrix
det :: Num a => Matrix n n a -> a
det (Single (Single a)) = a
det m                   = sum . vecHead $ m ..* (cBoardThenMOM m)

-- above two lines are virtually identical, just to make compiler happy
-- below are some convienience binary operators for matrices
(*.*) :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
(*.*) = multiplyMat

(..*) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
(..*) = matZipWith (*)

(..+) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
(..+) = matZipWith (+)
