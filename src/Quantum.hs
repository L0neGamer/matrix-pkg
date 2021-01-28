module Quantum where

import           Data.AdditiveGroup
import           Data.Complex       hiding ((:+))
import qualified Data.Complex       as C (Complex ((:+)))
import           Data.VectorSpace   hiding (magnitude)
import           Lib
import           Matrix
import           Vector            

-- base type for qubits
type Qubit = Matrix Two One (Complex Double)

-- for ease of use
sqone :: Floating a => a
sqone = 1 / sqrt 2

-- define some basic qubits/vectors
zero' :: (Num a, KnownNat n) => Matrix n One a
zero' = generateMat (\x _ -> fromIntegral $ fromEnum $ x == FZero)

zero :: Qubit
zero = zero'

one' :: (Num a, KnownNat n) => Matrix ('Succ n) One a
one' = generateMat (\x _ -> fromIntegral $ fromEnum $ x == (FSucc FZero))

one :: Qubit
one = one'

plus :: Qubit
plus = sqone *^ zero ^+^ sqone *^ one

minus :: Qubit
minus = sqone *^ zero ^-^ sqone *^ one

-- define some transformation matrices
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

rotation :: Floating a => a -> Matrix Two Two a
rotation n = Mat $ (c :+ singleton (-s)) :+ singleton (s :+ singleton c)
  where
    c = cos n
    s = sin n

cnot :: Num a => Matrix Four Four a
cnot = generateMat cnotFunc
  where
    cnotFunc m' n' =
      fromIntegral $ fromEnum $ (m <= 2 && m == n) || (m > 2 && n > 2 && m /= n)
      where
        (m, n) = (fromEnum m', fromEnum n')

-- define the tensor product
tensorProd ::
     (Num a, KnownNat n, KnownNat m, KnownNat i, KnownNat j)
  => Matrix n m a
  -> Matrix i j a
  -> Matrix (Mul n i) (Mul m j) a
tensorProd n m = expandNested $ fmap (*^ m) n

-- ease of use function for the tensor product
(.*.) ::
     (Num a, KnownNat n, KnownNat m, KnownNat i, KnownNat j)
  => Matrix n m a
  -> Matrix i j a
  -> Matrix (Mul n i) (Mul m j) a
(.*.) = tensorProd

infixr 8 .*.

-- transpose and get the conjugate of the given qubit
-- effectively, find the bra
conjTrans :: Qubit -> Matrix One Two (Complex Double)
conjTrans v = fmap conjugate $ Matrix.transpose v

-- using conjTrans, find the inner product of two qubits
innerProduct :: Qubit -> Qubit -> Complex Double
innerProduct v v' = vecHead . vecHead . getVec $ conjTrans v *.* v'

-- define the computational basis
compBasis :: Vector Two Qubit
compBasis = zero :+ singleton one

-- measure in a given basis, returning probabilities for each
-- basis vector
measureIn :: Qubit -> Vector n Qubit -> Vector n Double
measureIn v bs = fmap (\b -> (** 2) $ magnitude $ Quantum.innerProduct v b) bs

-- measure in the computational basis, returning probabilities
-- for each basis vector
measure :: Qubit -> Vector Two Double
measure v = measureIn v compBasis
