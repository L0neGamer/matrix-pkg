{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- module Matrix (Matrix, identity, mapMatrix, det, constructFrom) where
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
squareMatrix a v = constructFrom (\_ _ -> a) v v

constructFrom :: (a -> b -> c) -> Vector n a -> Vector m b -> Matrix n m c
constructFrom f (VSingle a) bs = singleton $ fmap (f a) bs
constructFrom f (VCons a as) bs = VCons (fmap (f a) bs) $ constructFrom f as bs

identity :: Num a => Vector n b -> Matrix n n a
identity vs =
  constructFrom
    (\a b -> fromIntegral $ fromEnum (a == b))
    indexVector
    indexVector
  where
    indexVector = (incrementingVec :: Vector n b -> Vector n Integer) vs

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
prependRow v m = v .:: m

appendCol :: Vector n a -> Matrix n m a -> Matrix n ('Succ m) a
appendCol v m = vecZipWith appendVal v m

prependCol :: Vector n a -> Matrix n m a -> Matrix n ('Succ m) a
prependCol v m = vecZipWith VCons v m

concatCols :: Matrix n m a -> Matrix n o a -> Matrix n (Add m o) a
concatCols = vecZipWith append

concatRows :: Matrix n m a -> Matrix o m a -> Matrix (Add n o) m a
concatRows = append

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

trace :: Num a => Matrix n m a -> a
trace (VCons (VSingle a) _)  = a
trace (VSingle v)            = vecHead v
trace (VCons (VCons a _) m') = a + trace (fromJust $ dropCol 0 m')

-- below are operations on matrices
-- transpose a nxm matrix to an mxn matrix
transpose :: Matrix n m a -> Matrix m n a
transpose (VSingle a) = fmap singleton a
transpose m@(VCons (VSingle _) _) = singleton $ fmap vecHead m
transpose (VCons v@(VCons _ _) vs) =
  vecZipWith VCons v $ VCons topRow (transpose tails)
  where
    tails = fmap vecTail vs
    topRow = fmap vecHead vs

-- zip together two equally sized matrices
matZipWith :: (a -> b -> c) -> Matrix n m a -> Matrix n m b -> Matrix n m c
matZipWith f a b = vecZipWith (vecZipWith f) a b

-- help from: https://github.com/janschultecom/idris-examples/blob/master/matrixmult.idr#L21
-- multiplies and sums together a pair of matrices
multVects :: Num a => Vector m a -> Vector m a -> a
multVects v1 v2 = sum $ vecZipWith (*) v1 v2

-- helper function to multiply a vector over a matrix
multVectMat :: Num a => Vector m a -> Matrix n m a -> Vector n a
multVectMat xs (VSingle vs) = singleton $ multVects xs vs
multVectMat xs (VCons v vs) = multVects xs v .:: multVectMat xs vs

-- multiply two matrices together
multiplyMat :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
multiplyMat (VSingle vs) b = singleton $ multVectMat vs $ transpose b
multiplyMat (VCons v vs) b = multVectMat v transposed .:: multiplyMat vs b
  where
    transposed = transpose b

-- helper function for finding determinants - alternating sum over a vector
altSum :: Num a => Vector n a -> a
altSum (VSingle a)            = a
altSum (VCons a (VSingle b))  = a - b
altSum (VCons a (VCons b vs)) = (a - b) + altSum vs

-- determinant of a 2x2 matrix
det2 :: Num a => Matrix Two Two a -> a
det2 (VCons (VCons a (VSingle b)) (VSingle (VCons c (VSingle d)))) =
  a * d - b * c
det2 _ = error "unreachable pattern for det2"

matrixOfMinors ::
     Num a
  => Matrix (Add Three n) (Add Three n) a
  -> Matrix (Add Three n) (Add Three n) a
matrixOfMinors m =
  mapMatrix (\(i, j) -> det $ fromJust $ subMatrix i j m) positions
  where
    indexVec = incrementingVec m
    positions = constructFrom (,) indexVec indexVec

checkerboard :: Num a => Matrix n m a -> Matrix n m a
checkerboard m = fmap (applyToRest negate) $ applyToRest (fmap negate) m

inverseMatrix' ::
     (Fractional a, Eq a)
  => Matrix (Add Three n) (Add Three n) a
  -> Maybe (Matrix (Add Three n) (Add Three n) a)
inverseMatrix' m
  | determinant == 0 = Nothing
  | otherwise =
    Just $ mapMatrix (/ determinant) (checkerboard $ matrixOfMinors m)
  where
    determinant = det m

-- thanks to https://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html
inverseMatrix ::
     (Fractional a, Eq a)
  => Matrix (Add Two n) (Add Two n) a
  -> Maybe (Matrix (Add Two n) (Add Two n) a)
inverseMatrix m@(VCons (VCons a (VSingle b)) (VSingle (VCons c (VSingle d))))
  | determinant == 0 = Nothing
  | otherwise =
    Just $
    mapMatrix (/ determinant) $
    (d .:: singleton (-b)) .:: singleton ((-c) .:: singleton a)
  where
    determinant = det m
inverseMatrix m@(VCons (VCons _ (VCons _ (VSingle _))) _) = inverseMatrix' m
inverseMatrix m@(VCons (VCons _ (VCons _ (VCons _ _))) _) = inverseMatrix' m
inverseMatrix _ = error "unreachable pattern for inverseMatrix"

-- helper for finding the determinant of a square matrix that is at least 3x3
det' :: Num a => Matrix (Add Three n) (Add Three n) a -> a
det' m@(VCons topRow _) = altSum multDetAndTop
  where
    dets = vecHead (matrixOfMinors m)
    multDetAndTop = vecZipWith (*) dets topRow

-- find the determinant for a square matrix
det :: Num a => Matrix (Add Two n) (Add Two n) a -> a
det m@(VCons (VCons _ (VSingle _)) _)           = det2 m
det m@(VCons (VCons _ (VCons _ (VSingle _))) _) = det' m
det m@(VCons (VCons _ (VCons _ (VCons _ _))) _) = det' m

-- above two lines are virtually identical, just to make compiler happy
-- below are some convienience binary operators for matrices
(*.*) :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
(*.*) = multiplyMat

(.*) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
(.*) = matZipWith (*)

(.+) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
(.+) = matZipWith (+)
