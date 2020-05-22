{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Matrix where
-- thanks to https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html

import Vector
import Lib

type Matrix2 n m a = Vector n (Vector m a)
-- n=rows, m=cols

singleton :: a -> Matrix2 One One a
singleton = Vector.singleton . Vector.singleton

-- transpose' :: Matrix2 n m a -> Matrix2 m n a
-- transpose' m@VNil = VNil
-- transpose' m@(VCons VNil _) = VNil
-- transpose' m@(VCons (VCons _ _) _) = transpose m

empties :: Matrix2 n Zero a
empties = VNil

transposeHelper :: Vector n a -> Matrix2 n m a -> Matrix2 n (Succ m) a
transposeHelper v m = binOpVec VCons v m

transpose :: Matrix2 n m a -> Matrix2 m n a
transpose VNil = VNil

-- transpose :: Matrix2 n m a -> Matrix2 m n a


-- transpose :: Matrix2 n m a -> Matrix2 m n a
-- transpose m@(VCons (VCons a VNil) VNil) = m
-- transpose m@(VCons v vs) = binOpVec VCons v $ VCons topRow (transpose tails)
--     where tails = fmap vecTail vs
--           topRow = fmap vecHead vs

-- applyNTimes :: Integral a => a -> (b -> b) -> b -> b
-- applyNTimes i f x
--     | i < 1 = x
--     | otherwise = applyNTimes (i - 1) f (f x)

appendRow :: Vector m a -> Matrix2 n m a -> Matrix2 (Add n One) m a
appendRow v m = append m (Vector.singleton v)

appendCol :: Vector n a -> Matrix2 n m a -> Matrix2 n (Add m One) a
appendCol VNil VNil = VNil
appendCol (VCons v vs) (VCons m ms) = append (Vector.singleton (append m (Vector.singleton v))) (appendCol vs ms)

squareMatrix2 :: a -> Matrix2 Two Two a
squareMatrix2 a = VCons row $ VCons row $ VNil
    where row = VCons a $ Vector.singleton a
squareMatrix3 :: a -> Matrix2 Three Three a
squareMatrix3 a = VCons row $ VCons row $ Vector.singleton row
    where row = VCons a $ VCons a $ Vector.singleton a

binOpMat :: (a -> b -> c) -> Matrix2 n m a -> Matrix2 n m b -> Matrix2 n m c
binOpMat f a b = binOpVec (binOpVec f) a b

-- multiplyMat :: Num a => Matrix2 n m a -> Matrix2 m o a -> Matrix2 n o a
-- multiplyMat a b = undefined
--     where rowHeads = fmap vecHead b
