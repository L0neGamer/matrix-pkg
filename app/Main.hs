module Main where

import Lib
import Vector
import Matrix hiding (singleton)

v1 :: Vector Five Int
v1 = 1 .:: 2 .:: 3 .:: 4 .:: singleton 5

iden5 :: Matrix2 Five Five Int
iden5 = identity v1

m1 :: Matrix2 Two Five Int
m1 = v1 .:: singleton v1

main :: IO ()
main = do
    putStrLn $ showVector v1
    print v1
    putStrLn $ showMatrix iden5
    putStrLn $ showMatrix m1
    putStrLn $ showMatrix (multiplyMat m1 iden5)
    putStrLn $ showMatrix (multiplyMat m1 (transpose m1))

