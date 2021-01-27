{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall                       #-}

module Quantum where

import           Data.Complex hiding ((:+))
import qualified Data.Complex as C (Complex ((:+)))
import           Lib
import           Matrix
import           Vector       hiding (dotProd)

sqone :: Floating a => a
sqone = 1 / sqrt 2

zero' :: (Num a, KnownNat n) => Matrix n 'One a
zero' = generateMat (\x _ -> fromIntegral $ fromEnum $ x == FZero)

zero :: (Num a) => Matrix Two 'One a
zero = zero'

one' :: (Num a, KnownNat n) => Matrix ('Succ n) 'One a
one' = generateMat (\x _ -> fromIntegral $ fromEnum $ x == (FSucc FZero))

one :: (Num a) => Matrix Two 'One a
one = one'

pauliX :: Num a => Matrix Two Two a
pauliX = Mat $ (0 :+ singleton 1) :+ singleton (1 :+ singleton 0)

pauliY :: RealFloat a => Matrix Two Two (Complex a)
pauliY =
  Mat $ (0 :+ singleton (0 C.:+ (-1))) :+ singleton ((0 C.:+ 1) :+ singleton 0)

pauliZ :: Num a => Matrix Two Two a
pauliZ = Mat $ (1 :+ singleton 0) :+ singleton (0 :+ singleton (-1))

hadamard :: Floating a => Matrix Two Two a
hadamard =
  fmap (* sqone) $ Mat $ (1 :+ singleton 1) :+ singleton (1 :+ singleton (-1))

cnot :: Num a => Matrix Four Four a
cnot = generateMat cnotFunc
  where
    cnotFunc m' n' =
      fromIntegral $ fromEnum $ (m <= 2 && m == n) || (m > 2 && n > 2 && m /= n)
      where
        (m, n) = (fromEnum m', fromEnum n')

tensorProd ::
     (Num a, KnownNat n, KnownNat m, KnownNat i, KnownNat j)
  => Matrix n m a
  -> Matrix i j a
  -> Matrix (Mul n i) (Mul m j) a
tensorProd n m = expandNested $ fmap (\c -> fmap (c *) m) n

(.*.) :: (Num a, KnownNat n, KnownNat m, KnownNat i, KnownNat j)
  => Matrix n m a
  -> Matrix i j a
  -> Matrix (Mul n i) (Mul m j) a
(.*.) = tensorProd
infixr 8 .*.
