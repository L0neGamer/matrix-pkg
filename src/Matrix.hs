{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Matrix where
-- thanks to https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
-- and to https://github.com/janschultecom/idris-examples for more complex examples

import Vector
import Lib

type Matrix2 n m a = Vector n (Vector m a)
-- n=rows, m=cols

showMatrix' :: Show a => Matrix2 n m a -> String
showMatrix' (VSingle vs) = showVector vs ++ "\n]"
showMatrix' (VCons vs vss) = showVector vs ++ ",\n " ++ showMatrix' vss

showMatrix :: Show a => Matrix2 n m a -> String
showMatrix vec = "[\n " ++ showMatrix' vec

singleton :: a -> Matrix2 'One 'One a
singleton = Vector.singleton . Vector.singleton

transposeHelper :: Vector n a -> Matrix2 n m a -> Matrix2 n ('Succ m) a
transposeHelper v m = binOpVec VCons v m

transpose :: Matrix2 n m a -> Matrix2 m n a
transpose (VSingle a) = fmap (Vector.singleton) a
transpose (VCons v@(VCons _ _) vs) = binOpVec VCons v $ VCons topRow (transpose tails)
    where tails = fmap vecTail vs
          topRow = fmap vecHead vs
transpose m@(VCons (VSingle _) _) = Vector.singleton $ fmap vecHead m

appendRow :: Vector m a -> Matrix2 n m a -> Matrix2 (Add n 'One) m a
appendRow v m = append m (Vector.singleton v)
prependRow :: Vector m a -> Matrix2 n m a -> Matrix2 (Add 'One n) m a
prependRow v m = append (Vector.singleton v) m

appendCol :: Vector n a -> Matrix2 n m a -> Matrix2 n (Add m 'One) a
appendCol v@(VSingle _) (VSingle vs) = VSingle (append vs v)
appendCol (VCons v vs) (VCons m ms) = append (Vector.singleton (append m (Vector.singleton v))) (appendCol vs ms)
prependCol :: Vector n a -> Matrix2 n m a -> Matrix2 n (Add 'One m) a
prependCol v@(VSingle _) (VSingle vs) = VSingle (append v vs)
prependCol (VCons v vs) (VCons m ms) = append (Vector.singleton (append (Vector.singleton v) m)) (prependCol vs ms)

squareMatrix2 :: a -> Matrix2 Two Two a
squareMatrix2 a = VCons row $ Vector.singleton row
    where row = VCons a $ Vector.singleton a
squareMatrix3 :: a -> Matrix2 Three Three a
squareMatrix3 a = VCons row $ VCons row $ Vector.singleton row
    where row = VCons a $ VCons a $ Vector.singleton a

binOpMat :: (a -> b -> c) -> Matrix2 n m a -> Matrix2 n m b -> Matrix2 n m c
binOpMat f a b = binOpVec (binOpVec f) a b

identity :: Num a => Vector n b -> Matrix2 n n a
identity (VSingle _) = Matrix.singleton 1
identity v@(VCons _ vs) = VCons (VCons 1 vs') ms
    where next = identity vs
          zeroed@(VCons _ zeroed') = fmap (\_ -> 0) v
          (VCons (VCons _ vs') ms) = prependRow zeroed $ prependCol zeroed' next

-- help from: https://github.com/janschultecom/idris-examples/blob/master/matrixmult.idr#L21
multVects :: Num a => Vector m a -> Vector m a -> a
multVects v1 v2 = sum $ binOpVec (*) v1 v2

multVectMat :: Num a => Vector m a -> Matrix2 n m a -> Vector n a
multVectMat xs (VSingle vs) = Vector.singleton $ multVects xs vs
multVectMat xs (VCons v vs) = multVects xs v .:: multVectMat xs vs

multiplyMat :: Num a => Matrix2 n m a -> Matrix2 m o a -> Matrix2 n o a
multiplyMat (VSingle vs) b = Vector.singleton $ multVectMat vs $ transpose b
multiplyMat (VCons v vs) b = multVectMat v transposed .:: multiplyMat vs b
    where transposed = transpose b

(**) :: Num a => Matrix2 n m a -> Matrix2 m o a -> Matrix2 n o a
m1 ** m2 = multiplyMat m1 m2
