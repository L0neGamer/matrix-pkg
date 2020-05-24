{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


-- module Matrix (Matrix, identity, mapMatrix, det, constructFrom) where
module Matrix where
-- thanks to https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
-- and to https://github.com/janschultecom/idris-examples for more complex examples

import Vector
import Lib

type Matrix n m a = Vector n (Vector m a)
-- n=rows, m=cols

-- below are construction and displaying matrices
squareMatrix :: a -> Vector n b -> Matrix n n a
squareMatrix a v = constructFrom (\_ _ -> a) v v

constructFrom :: (a -> b -> c) -> Vector n a -> Vector m b -> Matrix n m c
constructFrom f (VSingle a) bs = singleton $ fmap (f a) bs
constructFrom f (VCons a as) bs = VCons (fmap (f a) bs) $ constructFrom f as bs

identity :: Num a => Vector n b -> Matrix n n a
identity (VSingle _) = singleton $ singleton 1
identity v@(VCons _ vs) = VCons (VCons 1 vs') ms
    where next = identity vs
          zeroed@(VCons _ zeroed') = fmap (\_ -> 0) v
          (VCons (VCons _ vs') ms) = prependRow zeroed $ prependCol zeroed' next

mapMatrix :: (a -> b) -> Matrix n m a -> Matrix n m b
mapMatrix f m = fmap (fmap f) m

showMatrix' :: Show a => Matrix n m a -> String
showMatrix' (VSingle vs) = showVector vs ++ "\n]"
showMatrix' (VCons vs vss) = showVector vs ++ ",\n " ++ showMatrix' vss

showMatrix :: Show a => Matrix n m a -> String
showMatrix vec = "[\n " ++ showMatrix' vec

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

dropCol :: Integral a => a -> Matrix n ('Succ m) b -> Matrix n m b
dropCol a = fmap (dropItem a)
dropRow :: Integral a => a -> Matrix ('Succ n) m b -> Matrix n m b
dropRow a = dropItem a

-- drop the ith row and the jth column, or the last of either if out of bounds
subMatrix :: Integral a => a -> a -> Matrix ('Succ n) ('Succ m) b -> Matrix n m b
subMatrix i j = dropCol j . dropRow i

-- below are operations on matrices
-- transpose a nxm matrix to an mxn matrix
transpose :: Matrix n m a -> Matrix m n a
transpose (VSingle a) = fmap singleton a
transpose m@(VCons (VSingle _) _) = singleton $ fmap vecHead m
transpose (VCons v@(VCons _ _) vs) = vecZipWith VCons v $ VCons topRow (transpose tails)
    where tails = fmap vecTail vs
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
    where transposed = transpose b

-- helper function for finding determinants - alternating sum over a vector
altSum :: Num a => Vector n a -> a
altSum (VSingle a) = a
altSum (VCons a (VSingle b)) = a - b
altSum (VCons a (VCons b vs)) = (a - b) + altSum vs

-- determinant of a 2x2 matrix
det2 :: Num a => Matrix Two Two a -> a
det2 (VCons (VCons a (VSingle b)) (VSingle (VCons c (VSingle d)))) = a*d - b*c
det2 _ = error "unreachable pattern for det2"

-- helper for finding the determinant of a square matrix that is at least 3x3
det' :: Num a => Matrix (Add Three n) (Add Three n) a -> a
det' m@(VCons topRow _) = altSum multDetAndTop
    where indexVec = (incrementingVec topRow)
          matVec = fmap (\col -> subMatrix col (0::Integer) m) indexVec
          dets = fmap det matVec
          multDetAndTop = vecZipWith (*) dets topRow

-- find the determinant for a square matrix
det :: Num a => Matrix (Add Two n) (Add Two n) a -> a
det m@(VCons (VCons _ (VSingle _)) _) = det2 m
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

