module Main where

import Lib
import Vector
import Matrix
import Data.Foldable (toList)

v1 :: Vector Five Int
v1 = 1 .:: 2 .:: 3 .:: 4 .:: singleton 5

v2 :: Vector Two Int
v2 = 1 .:: singleton 2

iden5 :: Matrix Five Five Int
iden5 = identity v1

m1 :: Matrix Two Five Int
m1 = v1 .:: singleton v1

m2 :: Matrix Two Two Int
m2 = constructFrom (+) v2 v2

translate3 :: Num a => a -> a -> a -> Matrix Four Four a
translate3 a b c = (1 .:: 0 .:: 0 .:: singleton a) 
              .:: (0 .:: 1 .:: 0 .:: singleton b) 
              .:: (0 .:: 0 .:: 1 .:: singleton c) 
              .:: singleton (0 .:: 0 .:: 0 .:: singleton 1)
translate2 :: Num a => a -> a -> Matrix Three Three a
translate2 a b = (1 .:: 0 .:: singleton a) 
             .:: (0 .:: 1 .:: singleton b) 
             .:: singleton (0 .:: 0 .:: singleton 1)

scale2 :: Num a => a -> a -> Matrix Three Three a
scale2 a b = (a .:: 0 .:: singleton 0) 
         .:: (0 .:: b .:: singleton 0) 
         .:: singleton (0 .:: 0 .:: singleton 1)

rotation2 :: Floating a => a -> Matrix Three Three a
rotation2 a = (c .:: (-s) .:: singleton 0)
          .:: (s .:: c    .:: singleton 0)
          .:: singleton (0 .:: 0    .:: singleton 1)
    where c = cos a
          s = sin a

translation3Ex :: Matrix Four Four Double
translation3Ex = translate3 1 2 3


translation2Ex :: Matrix Three Three Double
translation2Ex = translate2 (-1) (-1)

scale2Ex :: Matrix Three Three Double
scale2Ex = scale2 2 2

rotation2Ex :: Matrix Three Three Double
rotation2Ex = rotation2 (pi/2)

main :: IO ()
main = do
    -- putStrLn $ showVector v1
    -- print v1
    -- putStrLn $ showMatrix iden5
    -- putStrLn $ showMatrix m1
    -- putStrLn $ showMatrix (multiplyMat m1 iden5)
    -- putStrLn $ showMatrix (multiplyMat m1 (transpose m1))
    -- putStrLn $ showMatrix (constructFrom (*) v1 v1)
    -- putStrLn $ showMatrix m2
    putStrLn $ showMatrix translation3Ex
    putStrLn $ showMatrix $ translation3Ex *.* (transpose $ singleton (0 .:: 0 .:: 0 .:: singleton 1))
    putStrLn $ showMatrix $ scale2Ex *.* (transpose $ singleton (1 .:: 1 .:: singleton 1))
    putStrLn $ showMatrix $ translation2Ex *.* rotation2Ex *.* scale2Ex *.* (transpose $ singleton (1 .:: 1 .:: singleton 1))
    putStrLn $ showMatrix $ translation2Ex *.* rotation2Ex *.* scale2Ex *.* (rotation2 (-pi/2))
    print $ v1 `dotProd` v1
    print $ (1 .:: singleton (-1)) `dotProd` (1 .:: singleton (1::Int))
    print $ toList $ crossProd (1 .:: 0 .:: singleton (0::Int)) (0 .:: 1 .:: singleton 0)
    -- print $ det m2

