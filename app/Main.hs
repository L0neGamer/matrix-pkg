{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Main where

import           Data.Foldable (toList)

import           Data.Maybe
import           Lib
import           Matrix
import           Vector

v1 :: Vector Five Integer
v1 = 1 :+ 2 :+ 3 :+ 4 :+ singleton 5

v2 :: Vector Two Integer
v2 = 1 :+ singleton 2

v3 :: Vector ('Succ Five) Integer
v3 = 0 :+ v1

iden5 :: Matrix Five Five Integer
iden5 = identity

m1 :: Matrix Two Five Integer
m1 = Mat $ v1 :+ singleton v1

m2 :: Matrix Two Two Integer
m2 = consFrom (+) v2 v2

m3 :: Matrix ('Succ Five) ('Succ Five) Integer
m3 =
  setAtMatrix (FSucc FZero) FZero 1 $ setAtMatrix FZero FZero 0 $
  generateMat (\x y -> cantorPairing (finSize x) (finSize y))

m4 :: Matrix Two Two Integer
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
    (transpose $ Mat $ singleton (0 :+ 0 :+ 0 :+ singleton 1))
  printMatrix $ scale2Ex *.*
    (transpose $ Mat $ singleton (1 :+ 1 :+ singleton 1))
  printMatrix $ translation2Ex *.* rotation2Ex *.* scale2Ex *.*
    (transpose $ Mat $ singleton (1 :+ 1 :+ singleton 1))
  printMatrix $ translation2Ex *.* rotation2Ex *.* scale2Ex *.*
    (rotation2 (-pi / 2))
  print $ v1 `dotProd` v1
  print $ (1 :+ singleton (-1)) `dotProd` (1 :+ singleton (1 :: Integer))
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
