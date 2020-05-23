{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
appendRow :: Vector m a -> Matrix n m a -> Matrix (Add n 'One) m a
appendRow v m = append m (singleton v)
prependRow :: Vector m a -> Matrix n m a -> Matrix (Add 'One n) m a
prependRow v m = append (singleton v) m

appendCol :: Vector n a -> Matrix n m a -> Matrix n (Add m 'One) a
appendCol v@(VSingle _) (VSingle vs) = VSingle (append vs v)
appendCol (VCons v vs) (VCons m ms) = append (singleton (append m (singleton v))) (appendCol vs ms)
prependCol :: Vector n a -> Matrix n m a -> Matrix n (Add 'One m) a
prependCol v@(VSingle _) (VSingle vs) = VSingle (append v vs)
prependCol (VCons v vs) (VCons m ms) = append (singleton (append (singleton v) m)) (prependCol vs ms)

-- below are operations on matrices
transpose :: Matrix n m a -> Matrix m n a
transpose (VSingle a) = fmap singleton a
transpose (VCons v@(VCons _ _) vs) = binOpVec VCons v $ VCons topRow (transpose tails)
    where tails = fmap vecTail vs
          topRow = fmap vecHead vs
transpose m@(VCons (VSingle _) _) = singleton $ fmap vecHead m

binOpMat :: (a -> b -> c) -> Matrix n m a -> Matrix n m b -> Matrix n m c
binOpMat f a b = binOpVec (binOpVec f) a b

-- help from: https://github.com/janschultecom/idris-examples/blob/master/matrixmult.idr#L21
multVects :: Num a => Vector m a -> Vector m a -> a
multVects v1 v2 = sum $ binOpVec (*) v1 v2

multVectMat :: Num a => Vector m a -> Matrix n m a -> Vector n a
multVectMat xs (VSingle vs) = singleton $ multVects xs vs
multVectMat xs (VCons v vs) = multVects xs v .:: multVectMat xs vs

multiplyMat :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
multiplyMat (VSingle vs) b = singleton $ multVectMat vs $ transpose b
multiplyMat (VCons v vs) b = multVectMat v transposed .:: multiplyMat vs b
    where transposed = transpose b

(*.*) :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
m1 *.* m2 = multiplyMat m1 m2

(.*) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
m1 .* m2 = binOpMat (*) m1 m2

(.+) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
m1 .+ m2 = binOpMat (+) m1 m2

