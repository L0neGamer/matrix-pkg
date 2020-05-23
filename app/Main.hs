module Main where

import Lib
import Vector
import Matrix

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

translate :: Num a => a -> a -> a -> Matrix Four Four a
translate a b c = (1 .:: 0 .:: 0 .:: singleton a) 
              .:: (0 .:: 1 .:: 0 .:: singleton b) 
              .:: (0 .:: 0 .:: 1 .:: singleton c) 
              .:: singleton (0 .:: 0 .:: 0 .:: singleton 1)

translationEx :: Matrix Four Four Double
translationEx = translate 1 2 3

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
    putStrLn $ showMatrix translationEx
    putStrLn $ showMatrix $ translationEx *.* (transpose $ singleton (0 .:: 0 .:: 0 .:: singleton 1))
    -- print $ det m2

