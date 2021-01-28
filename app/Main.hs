module Main where

import           Data.Foldable (toList)

import           Data.Maybe
import           Lib
import           Matrix
import           Quantum
import           Vector

v1 :: Vector Five Rational
v1 = 1 :+ 2 :+ 3 :+ 4 :+ singleton 5

v2 :: Vector Two Rational
v2 = 1 :+ singleton 2

v3 :: Vector Six Rational
v3 = 0 :+ v1

iden5 :: Matrix Five Five Rational
iden5 = identity

m1 :: Matrix Two Five Rational
m1 = Mat $ v1 :+ singleton v1

m2 :: Matrix Two Two Rational
m2 = consFrom (+) v2 v2

m3 :: Matrix Six Six Rational
m3 =
  fmap fromInteger $ setAtMatrix (FSucc FZero) FZero 1 $
  setAtMatrix FZero FZero 0 $
  generateMat (\x y -> cantorPairing (finSize x) (finSize y))

m4 :: Matrix Two Two Rational
m4 = Mat $ (1 :+ singleton 2) :+ singleton (3 :+ singleton 4)

m5 :: Matrix Two Two Double
m5 = Mat $ (4 :+ singleton 7) :+ singleton (2 :+ singleton 6)

translate3 :: Num a => a -> a -> a -> Matrix Four Four a
translate3 a b c =
  Mat $ (1 :+ 0 :+ 0 :+ singleton a) :+ (0 :+ 1 :+ 0 :+ singleton b) :+
  (0 :+ 0 :+ 1 :+ singleton c) :+
  singleton (0 :+ 0 :+ 0 :+ singleton 1)

translate2 :: Num a => a -> a -> Matrix Three Three a
translate2 a b =
  Mat $ (1 :+ 0 :+ singleton a) :+ (0 :+ 1 :+ singleton b) :+
  singleton (0 :+ 0 :+ singleton 1)

scale2 :: Num a => a -> a -> Matrix Three Three a
scale2 a b =
  Mat $ (a :+ 0 :+ singleton 0) :+ (0 :+ b :+ singleton 0) :+
  singleton (0 :+ 0 :+ singleton 1)

rotation2 :: Floating a => a -> Matrix Three Three a
rotation2 a =
  Mat $ (c :+ (-s) :+ singleton 0) :+ (s :+ c :+ singleton 0) :+
  singleton (0 :+ 0 :+ singleton 1)
  where
    c = cos a
    s = sin a

translation3Ex :: Matrix Four Four Double
translation3Ex = translate3 1 2 3

translation2Ex :: Matrix Three Three Double
translation2Ex = translate2 (-1) (-1)

scale2Ex :: Matrix Three Three Double
scale2Ex = scale2 2 2

rotation2Ex :: Matrix Three Three Double
rotation2Ex = rotation2 (pi / 2)

mt123 :: Matrix Four Four Double
mt123 = translate3 1 2 3

m6 :: Matrix Three Three Double
m6 =
  Mat $ (3 :+ 0 :+ singleton 2) :+ (2 :+ 0 :+ singleton (-2)) :+
  (singleton (0 :+ 1 :+ singleton 1))

m7 :: Matrix Six Six Rational
m7 = setAtMatrix maxBound FZero 14 $ setCol FZero (generate (\_ -> 0)) m3

main :: IO ()
main = do
  printMatrix iden5
    -- putStrLn $ showMatrix m1
    -- putStrLn $ showMatrix (multiplyMat m1 iden5)
    -- putStrLn $ showMatrix (multiplyMat m1 (transpose m1))
    -- putStrLn $ showMatrix (consFrom (*) v1 v1)
    -- putStrLn $ showMatrix m2
  printMatrix translation3Ex
  printMatrix $ translation3Ex *.*
    (Matrix.transpose $ Mat $ singleton (0 :+ 0 :+ 0 :+ singleton 1))
  printMatrix $ scale2Ex *.*
    (Matrix.transpose $ Mat $ singleton (1 :+ 1 :+ singleton 1))
  printMatrix $ translation2Ex *.* rotation2Ex *.* scale2Ex *.*
    (Matrix.transpose $ Mat $ singleton (1 :+ 1 :+ singleton 1))
  printMatrix $ translation2Ex *.* rotation2Ex *.* scale2Ex *.*
    (rotation2 (-pi / 2))
  print $ v1 Vector.<.> v1
  print $ (1 :+ singleton (-1)) Vector.<.> (1 :+ singleton (1 :: Integer))
  print $ toList $
    crossProd (1 :+ 0 :+ singleton (0 :: Integer)) (0 :+ 1 :+ singleton 0)
  printMatrix m4
  print $ det m4
  printMatrix $ m5
  printMatrix $ fromJust $ inverseMatrix m5
  printMatrix $ mt123
  print $ det $ mt123
  printMatrix $ fromJust $ inverseMatrix $ mt123
  printMatrix m6
  print $ det m6
  printMatrix $ fromJust $ inverseMatrix m6
  putStrLn "Starting Rank testing"
  let doPrint mat =
        (do printMatrix mat
            printMatrix $ fst $ rank' mat)
  doPrint m1
  doPrint m2
  doPrint m3
  doPrint m4
  doPrint m5
  doPrint m6
  doPrint m7
