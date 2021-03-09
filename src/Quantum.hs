{-# LANGUAGE AllowAmbiguousTypes #-}

module Quantum where

import           Data.AdditiveGroup
import           Data.Complex       hiding ((:+))
import qualified Data.Complex       as C (Complex ((:+)))
import           Data.VectorSpace   hiding (magnitude)
import           Lib
import           Matrix
import           Vector

-- base type for qubits
type CDouble = Complex Double
type CVVec n = VVec n CDouble
type Qubit = CVVec Two
type NQubits n = CVVec (Exp Two n)

-- for ease of use
sqtwo :: Floating a => a
sqtwo = 1 / sqrt 2

i :: Num a => Complex a
i = 0 C.:+ 1

-- define some basic qubits/vectors, and qubit collections
basisVec :: (Num a, KnownNat n) => Fin n -> VVec n a
basisVec f = generateMat (\x _ -> fromIntegral $ fromEnum $ x == f)

zero' :: (Num a, KnownNat n) => VVec n a
zero' = basisVec FZero

zero :: Qubit
zero = zero'

one' :: (Num a, KnownNat n) => VVec ( 'Succ n) a
one' = basisVec (FSucc FZero)

one :: Qubit
one = one'

plus :: Qubit
plus = sqtwo *^ zero ^+^ sqtwo *^ one

minus :: Qubit
minus = sqtwo *^ zero ^-^ sqtwo *^ one

qubits :: [Qubit]
qubits = [zero, one, plus, minus]

makeTwoQubits :: [Qubit] -> [NQubits Two]
makeTwoQubits qs = (.*.) <$> qs <*> qs

twoQubits :: [NQubits Two]
twoQubits = makeTwoQubits [zero, one]

consQubit :: CDouble -> CDouble -> Qubit
consQubit a b = numMatFromList [[a], [b]]

-- define some transformation matrices
pauliX :: Num a => Matrix Two Two a
pauliX = numMatFromList [[0, 1], [1, 0]]

pauliY :: RealFloat a => Matrix Two Two (Complex a)
pauliY = numMatFromList [[0, -i], [i, 0]]

pauliZ :: Num a => Matrix Two Two a
pauliZ = numMatFromList [[1, 0], [0, -1]]

hadamard :: Floating a => Matrix Two Two a
hadamard = fmap (* sqtwo) $ numMatFromList [[1, 1], [1, -1]]

-- a rotation matrix. produces a matrix which rotates by n radians
rotation :: Floating a => a -> Matrix Two Two a
rotation n = numMatFromList [[c, -s], [s, c]]
 where
  c = cos n
  s = sin n

cnot :: Num a => Matrix Four Four a
cnot = numMatFromList [[1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]]

cnot' :: Num a => Matrix Four Four a
cnot' = numMatFromList [[1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 1, 0, 0]]

iden2 :: Num a => Matrix Two Two a
iden2 = identity

swap :: Num a => Matrix Four Four a
swap =
  Mat
    $  (1 :+ 0 :+ 0 :+ singleton 0)
    :+ (0 :+ 0 :+ 1 :+ singleton 0)
    :+ (0 :+ 1 :+ 0 :+ singleton 0)
    :+ singleton ((0 :+ 0 :+ 0 :+ singleton 1))

-- define the tensor product
tensorProd
  :: (Num a) => Matrix n m a -> Matrix i j a -> Matrix (Mul n i) (Mul m j) a
tensorProd n m = expandNested $ fmap (\a -> fmap (a *) m) n

-- ease of use function for the tensor product
(.*.) :: (Num a) => Matrix n m a -> Matrix i j a -> Matrix (Mul n i) (Mul m j) a
(.*.) = tensorProd

infixr 8 .*.

-- apply the tensor product a given number of times.
-- to use this function, use `tensorPower @(type number) matrix`
-- for example, to tensor product matrix `a` gate three times, do `tensorPower @Three a`
tensorPower
  :: forall i a n m
   . (Num a, KnownNat i)
  => Matrix n m a
  -> Matrix (Exp n i) (Exp m i) a
tensorPower m = tensorPower' m (natSing :: NatS i)

tensorPower' :: Num a => Matrix n m a -> NatS i -> Matrix (Exp n i) (Exp m i) a
tensorPower' m (OneS   ) = m
tensorPower' m (SuccS s) = m .*. (tensorPower' m s)

-- transpose and get the conjugate of the given qubit
-- effectively, find the bra
conjTrans :: CVVec n -> HVec n CDouble
conjTrans v = fmap conjugate $ Matrix.transpose v

-- using conjTrans, find the inner product of two qubits
innerProduct :: CVVec n -> CVVec n -> CDouble
innerProduct v v' = getVal $ conjTrans v *.* v'

compBasis' :: (Num a, KnownNat n) => Vector n (VVec n a)
compBasis' = generateVec basisVec

-- define the computational basis
compBasis :: Vector Two Qubit
compBasis = compBasis'

transformBasis :: (Num a, KnownNat n) => Matrix n n a -> Vector n (VVec n a)
transformBasis m = fmap (m *.*) compBasis'

-- measure in a given basis, returning probabilities for each
-- basis vector
measureIn :: CVVec m -> Vector n (CVVec m) -> Vector n Double
measureIn v bs = fmap (\b -> (** 2) $ magnitude $ Quantum.innerProduct v b) bs

-- measure in the computational basis, returning probabilities
-- for each basis vector
measure :: Qubit -> Vector Two Double
measure v = measureIn v compBasis

-- given a list of vector-probability pairs, return Just the density matrix
--  of all if the probabilities sum up to 1; else return Nothing
getDensityMatrix
  :: (KnownNat m) => [(CVVec m, CDouble)] -> Maybe (Matrix m m CDouble)
getDensityMatrix qs
  | sum (map snd qs) == 1 = Just
  $ foldr (^+^) zeroed [ p *^ (f tq) | (tq, p) <- qs ]
  | otherwise = Nothing
 where
  zeroed = generateMat (\_ _ -> 0)
  f m = m *.* conjTrans m

-- given a probability matrix (as seen in previous function),
--  and a complex number vector, return the probability of
--  seeing that vector
calcProbability :: Matrix m m CDouble -> CVVec m -> CDouble
calcProbability densityMatrix base =
  getVal $ conjTrans base *.* densityMatrix *.* base
