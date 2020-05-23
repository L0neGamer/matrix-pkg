{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


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

-- below are operations on matrices
transpose :: Matrix n m a -> Matrix m n a
transpose (VSingle a) = fmap singleton a
transpose (VCons v@(VCons _ _) vs) = vecZipWith VCons v $ VCons topRow (transpose tails)
    where tails = fmap vecTail vs
          topRow = fmap vecHead vs
transpose m@(VCons (VSingle _) _) = singleton $ fmap vecHead m

matZipWith :: (a -> b -> c) -> Matrix n m a -> Matrix n m b -> Matrix n m c
matZipWith f a b = vecZipWith (vecZipWith f) a b

-- help from: https://github.com/janschultecom/idris-examples/blob/master/matrixmult.idr#L21
multVects :: Num a => Vector m a -> Vector m a -> a
multVects v1 v2 = sum $ vecZipWith (*) v1 v2

multVectMat :: Num a => Vector m a -> Matrix n m a -> Vector n a
multVectMat xs (VSingle vs) = singleton $ multVects xs vs
multVectMat xs (VCons v vs) = multVects xs v .:: multVectMat xs vs

multiplyMat :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
multiplyMat (VSingle vs) b = singleton $ multVectMat vs $ transpose b
multiplyMat (VCons v vs) b = multVectMat v transposed .:: multiplyMat vs b
    where transposed = transpose b

-- {- 
-- Expected type: Vector o (Matrix m (Add m ('Succ o)) a)
--   Actual type: Vector o (Matrix n (Add ('Succ m) o) a)
-- -}

-- det''' :: Matrix n m a -> Vector n a -> Matrix n ('Succ o) a -> Vector ('Succ o) (Matrix n (Add m ('Succ o)) a)
-- det''' mn vs m@(VCons (VCons _ _) _) = VCons (concatCols mn m) (det'' mn' vs' m')
--     where mn' = appendCol vs mn
--           vs' = fmap vecHead m
--           m' = fmap vecTail m

-- det'' :: Matrix n m a -> Vector n a -> Matrix n o a -> Vector o (Matrix n (Add m o) a)
-- det'' mn vs m@(VCons (VSingle _) _) = VSingle (concatCols mn m)
-- det'' mn vs m@(VCons (VCons _ _) _) = det''' mn vs m
-- -- det'' mn vs m@(VCons (VCons _ _) _) = VCons (concatCols mn m) (det'' mn' vs' m')
-- --     where mn' = appendCol vs mn
-- --           vs' = fmap vecHead m
-- --           m' = fmap vecTail m

-- det' :: Num a => Matrix ('Succ n) ('Succ ('Succ n)) a -> Vector ('Succ n) (Matrix ('Succ n) ('Succ n) a)
-- det' m = VCons (fmap vecTail m) $ det'' (fmap (VSingle . vecHead) m) (fmap (vecHead . vecTail) m) (fmap (vecTail . vecTail) m)

-- det :: Num a => Matrix n n a -> a
-- det (VSingle (VSingle a)) = a

(*.*) :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
m1 *.* m2 = multiplyMat m1 m2

(.*) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
m1 .* m2 = matZipWith (*) m1 m2

(.+) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
m1 .+ m2 = matZipWith (+) m1 m2

