module Lib where

import Data.Foldable (find, toList)
import Data.Kind
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import Prelude hiding (zipWith)

{-
Nat is for keeping the size of the vector in the type.
this is done via promotion via... DataKinds I think
Think of 'Nat and friends as the Types of this setup

:k Nat   == Nat :: *
:k One   == One :: Nat
:t One   == One :: Nat
:k Succ  == Succ :: Nat -> Nat
:t Succ  == Succ :: Nat -> Nat
So the above definitions show that a Nat is a plain type; it takes no arguments
-}
data Nat
  = One
  | Succ Nat
  deriving (Show, Eq)

-- Following are KnownNats, which help with automatic construction of things
data NatS :: Nat -> Type where
  OneS :: NatS 'One
  SuccS :: KnownNat n => NatS n -> NatS ('Succ n)

deriving instance Show (NatS a)

deriving instance Eq (NatS a)

class KnownNat (n :: Nat) where
  natSing :: NatS n

instance KnownNat 'One where
  natSing = OneS

instance KnownNat n => KnownNat ('Succ n) where
  natSing = SuccS natSing

-- Fin is for indexing through the data structures.
-- think of it as the "Numbers" of these structures
-- :k Fin   == Fin :: Nat -> *
-- :k FZero == FZero :: Fin n
-- :t FZero == FZero :: Fin n
-- :k FSucc == FSucc :: Fin n -> Fin ('Succ n)
-- :t FSucc == FSucc :: Fin n -> Fin ('Succ n)
data Fin :: Nat -> Type where
  FZero :: Fin n
  FSucc :: Fin n -> Fin ('Succ n)

deriving instance Show (Fin n)

deriving instance Eq (Fin n)

deriving instance Ord (Fin n)

type One = 'One

type Two = 'Succ 'One

type Three = 'Succ Two

type Four = 'Succ Three

type Five = 'Succ Four

type Six = 'Succ Five

type Seven = 'Succ Six

type Eight = 'Succ Seven

type Nine = 'Succ Eight

type Ten = 'Succ Nine

type family GT (n :: Nat) (m :: Nat) where
  GT ('Succ _) 'One = 'True
  GT 'One ('Succ _) = 'False
  GT ('Succ n) ('Succ m) = GT n m

type family EQ (n :: Nat) (m :: Nat) where
  EQ 'One 'One = 'True
  EQ ('Succ _) 'One = 'False
  EQ 'One ('Succ _) = 'False
  EQ ('Succ n) ('Succ m) = EQ n m

type family Or (n :: Bool) (m :: Bool) where
  Or 'True _ = 'True
  Or _ 'True = 'True
  Or _ _ = 'False

type family And (n :: Bool) (m :: Bool) where
  And 'True 'True = 'True
  And _ _ = 'False

type family Not (n :: Bool) where
  Not 'True = 'False
  Not 'False = 'True

type family IsEven (n :: Nat) where
  IsEven ('Succ 'One) = 'True
  IsEven ('Succ ('Succ s)) = IsEven s
  IsEven 'One = 'False

type family GTE (n :: Nat) (m :: Nat) where
  GTE n m = Or (GT n m) (EQ n m)

--  look at https://wiki.haskell.org/Type_arithmetic
-- http://archive.fo/JwMNI
type family Add n m where
  Add 'One n = 'Succ n
  Add ('Succ n) m = 'Succ (Add n m)

-- Multiplication, allowing ever more powerful operators
type family Mul n m where
  Mul 'One m = m
  Mul ('Succ n) m = Add m (Mul n m)

-- Exponentiation, allowing even even more powerful operators
type family Exp n m where
  Exp n 'One = n
  Exp n ('Succ m) = Mul n (Exp n m)

-- this allows for use of inference of the exponential type
--  thanks to finnbar and dixonary!
type family RLog n m x i where
  RLog n m m i = i
  RLog n m x i = RLogBool n m x i (GT m x)

-- this is a helper type so that we can error out properly
type family RLogBool n m x i (mx :: Bool) where
  RLogBool n m x i 'True = (RLog n m (Mul n x) ('Succ i))
  RLogBool n m x i 'False =
    TypeError
      ( 'Text "Recursed too deep! Base ("
          ':<>: 'ShowType n
          ':<>: 'Text ") does not divide evenly into ("
          ':<>: 'ShowType m
          ':<>: 'Text ")"
      )

-- wrapping some stuff up in syntactic sugar
type ReverseLog n m = RLog n m n 'One

-- the thing we wanna constrain on
type GetExp n i = ReverseLog n (Exp n i)

type family Sub (n :: Nat) (m :: Nat) where
  Sub 'One 'One = TypeError ('Text "Cannot subtract equal numbers!")
  Sub 'One ('Succ _) = TypeError ('Text "Cannot subtract a larger number from a smaller number!")
  Sub ('Succ s) 'One = s
  Sub ('Succ s) ('Succ s') = Sub s s'

type family IsDivisibleBy (n :: Nat) (m :: Nat) where
  IsDivisibleBy _ 'One = 'True
  IsDivisibleBy n m =
    IfElse
      (EQ n m)
      'True
      (IfElse (GT n m) (IsDivisibleBy (Sub n m) m) 'False)

type family IfElse (b :: Bool) n m where
  IfElse 'True n _ = n
  IfElse 'False _ m = m

-- ways to generate minimums and maximums of Fins
instance (KnownNat n) => Bounded (Fin n) where
  minBound = FZero
  maxBound =
    case natSing @n of
      OneS -> FZero
      SuccS _ -> FSucc maxBound

-- converting to and from Ints for Fins
-- a bit unsafe, but that's fine, since it's
-- supposed to be
instance (KnownNat n) => Enum (Fin n) where
  fromEnum = finSize
  toEnum 1 = FZero
  toEnum n
    | n > 1 =
      case natSing @n of
        OneS -> err
        SuccS _ -> FSucc (toEnum (n - 1))
    | otherwise = err
    where
      err = error $ "bad Int for toEnum in Finn: " ++ show n

-- define my own type class for operators between two
-- types
class LinearData v where
  (^*^) :: Num a => v a -> v a -> v a
  (^*^) = zipWith (*)
  zipWith :: (a -> b -> c) -> v a -> v b -> v c

-- get the numerical representation of a Fin
finSize :: Num a => Fin n -> a
finSize FZero = 1
finSize (FSucc f) = 1 + finSize f

-- create a list of Fins equal to the size of the type in
-- ascending order
fins :: forall n. KnownNat n => [Fin n]
fins = map toEnum $ take (fromEnum (maxBound :: Fin n)) [1, 2 ..]

-- get an ascending list of Fins greater than the given Fin
finFrom :: forall n. KnownNat n => Fin n -> [Fin n]
finFrom from = dropWhile (< from) fins

-- convert a NatS into a number
natSSize :: Num a => NatS n -> a
natSSize OneS = 1
natSSize (SuccS s) = 1 + natSSize s

-- thank you to dixonary at https://discord.com/channels/189453139584221185/231852430701232128/806896486860324894
getAt :: (Foldable t, Integral b) => b -> t a -> a -> a
getAt i xs def = maybe def snd $ find ((== i) . fst) $ zip [0 ..] $ toList xs

-- below are some extra functions which are just useful for testing
cantorPairing :: Integral a => a -> a -> a
cantorPairing a b = div ((a + b) * (a + b + 1)) 2 + b

inverseCantorPairing :: Integral a => a -> (a, a)
inverseCantorPairing a = (w - a + t, a - t)
  where
    w = floor (sqrt (8 * fromIntegral a + 1) - 1 / 2 :: Double)
    t = div (w ^ (2 :: Int) + w) 2

padList :: a -> Int -> [a] -> [a]
padList a i as = replicate i a ++ as
