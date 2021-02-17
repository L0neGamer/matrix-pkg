module Quantum where

import           Data.AdditiveGroup
import           Data.Complex       hiding ((:+))
import qualified Data.Complex       as C (Complex ((:+)))
import           Data.VectorSpace   hiding (magnitude)
import           Lib
import           Matrix
import           Vector

-- base type for qubits
type CVVec n = VVec n (Complex Double)
type Qubit = CVVec Two
type TwoQubit = CVVec Four

-- for ease of use
sqtwo :: Floating a => a
sqtwo = 1 / sqrt 2

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

makeTwoQubits :: [Qubit] -> [TwoQubit]
makeTwoQubits qs = (.*.) <$> qs <*> qs

twoQubits :: [TwoQubit]
twoQubits = makeTwoQubits [zero, one]

consQubit :: Complex Double -> Complex Double -> Qubit
consQubit a b = Mat $ (singleton a) :+ (singleton $ singleton b)

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
  fmap (* sqtwo) $ Mat $ (1 :+ singleton 1) :+ singleton (1 :+ singleton (-1))

-- a rotation matrix. produces a matrix which rotates by n radians
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
    where (m, n) = (fromEnum m', fromEnum n')

cnot' :: Num a => Matrix Four Four a
cnot' =
  Mat
    $  (1 :+ 0 :+ 0 :+ singleton 0)
    :+ (0 :+ 0 :+ 0 :+ singleton 1)
    :+ (0 :+ 0 :+ 1 :+ singleton 0)
    :+ singleton ((0 :+ 1 :+ 0 :+ singleton 0))

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
  :: (Num a, KnownNat n, KnownNat m, KnownNat i, KnownNat j)
  => Matrix n m a
  -> Matrix i j a
  -> Matrix (Mul n i) (Mul m j) a
tensorProd n m = expandNested $ fmap (*^ m) n

-- ease of use function for the tensor product
(.*.)
  :: (Num a, KnownNat n, KnownNat m, KnownNat i, KnownNat j)
  => Matrix n m a
  -> Matrix i j a
  -> Matrix (Mul n i) (Mul m j) a
(.*.) = tensorProd

infixr 8 .*.

-- transpose and get the conjugate of the given qubit
-- effectively, find the bra
conjTrans :: CVVec n -> HVec n (Complex Double)
conjTrans v = fmap conjugate $ Matrix.transpose v

-- using conjTrans, find the inner product of two qubits
innerProduct :: CVVec n -> CVVec n -> Complex Double
innerProduct v v' = getVal $ conjTrans v *.* v'

compBasis' :: (Num a, KnownNat n) => Vector n (VVec n a)
compBasis' = generate basisVec

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

getDensityMatrix
  :: (KnownNat m)
  => [(CVVec m, Complex Double)]
  -> Maybe (Matrix m m (Complex Double))
getDensityMatrix qs
  | sum (map snd qs) == 1 = Just
  $ foldr (^+^) zeroed [ p *^ (f tq) | (tq, p) <- qs ]
  | otherwise = Nothing
 where
  zeroed = generateMat (\_ _ -> 0)
  f m = m *.* conjTrans m

calcProbability :: Matrix m m (Complex Double) -> CVVec m -> Complex Double
calcProbability densityMatrix base =
  getVal $ conjTrans base *.* densityMatrix *.* base
