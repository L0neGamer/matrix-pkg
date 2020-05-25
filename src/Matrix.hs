{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall                       #-}

-- module Matrix (Matrix, identity, mapMatrix, det, consFrom) where
module Matrix where

import           Data.Foldable   (toList)
import           Data.Singletons
import           Lib
import           Vector

-- thanks to https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
-- and to https://github.com/janschultecom/idris-examples for more complex examples
type Matrix n m a = Vector n (Vector m a)

-- below are construction and displaying matrices
squareMatrix :: a -> Vector n b -> Matrix n n a
squareMatrix a v = consFrom (\_ _ -> a) v v

generateMat_ :: (SingI m) => Sing n -> (Fin n -> Fin m -> a) -> Matrix n m a
generateMat_ SOne f       = VecSing (generate (f FZero))
generateMat_ (SSucc ss) f = generate (f FZero) :+ generateMat_ ss (f . FSucc)

generateMat :: (SingI n, SingI m) => (Fin n -> Fin m -> a) -> Matrix n m a
generateMat = generateMat_ sing

consFrom :: (a -> b -> c) -> Vector n a -> Vector m b -> Matrix n m c
consFrom f (VecSing a) bs = singleton $ fmap (f a) bs
consFrom f (a :+ as) bs   = fmap (f a) bs :+ consFrom f as bs

-- consFromCoord ::
--      (Integer -> Integer -> a) -> Vector n b -> Vector m c -> Matrix n m a
-- consFromCoord f as bs = mapMatrix (\(x, y) -> f x y) positions
--   where
--     (indexVecA, indexVecB) = (incrementingVec as, incrementingVec bs)
--     positions = consFrom (,) indexVecA indexVecB
identity :: (Num a, SingI n) => Matrix n n a
identity = generateMat (\a b -> fromIntegral $ fromEnum (a == b))

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

dropCol :: Fin ('Succ m) -> Matrix n ('Succ m) b -> Matrix n m b
dropCol a = fmap (dropIndex a)

-- dropCol :: Integer -> Matrix n ('Succ m) b -> Maybe (Matrix n m b)
-- dropCol a = sequence . fmap (dropItem a)
dropRow :: Fin ('Succ n) -> Matrix ('Succ n) m b -> Matrix n m b
dropRow a = dropIndex a

-- dropRow :: Integer -> Matrix ('Succ n) m b -> Maybe (Matrix n m b)
-- dropRow a = dropItem a
setAtMatrix :: Fin n -> Fin m -> a -> Matrix n m a -> Matrix n m a
setAtMatrix i j a m = replace i (replace j a (index i m)) m

-- setAtMatrix :: Integer -> Integer -> a -> Matrix n m a -> Maybe (Matrix n m a)
-- setAtMatrix i j a m = getAt i m >>= setAt j a >>= \col' -> setAt i col' m
getAtMatrix :: Fin n -> Fin m -> Matrix n m a -> a
getAtMatrix i j = index j . index i

-- getAtMatrix :: Integer -> Integer -> Matrix n m a -> Maybe a
-- getAtMatrix i j m = getAt i m >>= getAt j
-- drop the ith row and the jth column, or the last of either if out of bounds
-- subMatrix ::
--      Integer -> Integer -> Matrix ('Succ n) ('Succ m) b -> Maybe (Matrix n m b)
-- subMatrix i j m = dropRow i m >>= dropCol j
subMatrix ::
     Fin ('Succ n)
  -> Fin ('Succ m)
  -> Matrix ('Succ n) ('Succ m) b
  -> Matrix n m b
subMatrix i j = dropCol j . dropRow i

-- trace :: (Num a, SingI n) => Matrix n n a -> a
-- trace (VecSing (VecSing a)) = a
-- trace m@((a :+ _) :+ _)     = a + trace (subMatrix FZero FZero m)
-- below are operations on matrices
-- transpose a nxm matrix to an mxn matrix
transpose :: Matrix n m a -> Matrix m n a
transpose (VecSing a) = fmap singleton a
transpose m@((VecSing _) :+ _) = singleton $ fmap vecHead m
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
multVectMat xs (VecSing v) = singleton $ dotProd xs v
multVectMat xs (v :+ vs)   = dotProd xs v :+ multVectMat xs vs

-- multiply two matrices together
multiplyMat :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
multiplyMat (VecSing vs) b = (singleton . multVectMat vs . transpose) b
multiplyMat (v :+ vs) b    = multVectMat v (transpose b) :+ multiplyMat vs b

-- matrixOfMinors :: (Num a, SingI ('Succ n), SingI n) => Matrix ('Succ n) ('Succ n) a -> Matrix ('Succ n) ('Succ n) a
-- matrixOfMinors ((a :+ VecSing b) :+ (VecSing (c :+ VecSing d))) = ((d :+ VecSing c) :+ (VecSing (b :+ VecSing a)))
-- matrixOfMinors m@(_ :+ (_ :+ _)) = mapMatrix det $ generateMat (\i j -> subMatrix i j m)
checkerboard :: Num a => Matrix n m a -> Matrix n m a
checkerboard = fmap (applyToRest negate) . applyToRest (fmap negate)

-- cBoardThenMOM :: (Num a, SingI ('Succ n), SingI n) => Matrix ('Succ n) ('Succ n) a -> Matrix ('Succ n) ('Succ n) a
-- cBoardThenMOM = checkerboard . matrixOfMinors
-- thanks to https://www.mathsisfun.com/algebra/matrix-inverse-minors-cofactors-adjugate.html
-- inverseMatrix :: (Fractional a, Eq a, SingI n, SingI ('Succ n)) => Matrix ('Succ n) ('Succ n) a -> Maybe (Matrix ('Succ n) ('Succ n) a)
-- -- inverseMatrix (VecSing (VecSing 0)) = Nothing
-- -- inverseMatrix (VecSing (VecSing a)) = Just (VecSing (VecSing (recip a)))
-- inverseMatrix m
--   | determinant == 0 = Nothing
--   | otherwise = Just $ transpose $ mapMatrix (/ determinant) (cBoardThenMOM m)
--   where
--     determinant = det m
-- find the determinant for a square matrix
-- det :: (Num a, SingI ('Succ n), SingI n) => Matrix ('Succ n) ('Succ n) a -> a
-- det ((a :+ VecSing b) :+ (VecSing (c :+ VecSing d))) = a*d - b*c
-- det m@(_ :+ (_ :+ _))                     = sum . vecHead $ m ..* (cBoardThenMOM m)
-- above two lines are virtually identical, just to make compiler happy
-- below are some convienience binary operators for matrices
(*.*) :: Num a => Matrix n m a -> Matrix m o a -> Matrix n o a
(*.*) = multiplyMat

(..*) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
(..*) = matZipWith (*)

(..+) :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
(..+) = matZipWith (+)
