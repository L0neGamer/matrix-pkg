{-# LANGUAGE PatternSynonyms #-}

module Fin (Fin, mkFin, mkFinErr, unFin, pattern FNat, pattern FZero, reduceFin) where

import GHC.TypeNats as TN
import Data.Proxy
import GHC.Natural (Natural)

data Fin (n :: Nat) where
  Fin :: Natural -> Fin n

mkFin :: forall a n. (Integral a, Ord a, KnownNat n) => a -> Maybe (Fin n)
mkFin a | a' >= 0 && a' < i = Just $ Fin a'
        | otherwise = Nothing
  where i = natVal (Proxy @n)
        a' = fromIntegral a

mkFinErr :: forall a n. (Integral a, Ord a, KnownNat n) => a -> Fin n
mkFinErr a | a' >= 0 && a' < i = Fin a'
           | otherwise = error $ "mkFinErr could not construct Fin from " <> show a' <> "; upper bound is " <> show i
  where i = natVal (Proxy @n)
        a' = fromIntegral a

unFin :: Fin n -> Natural
unFin (Fin n) = n

pattern FNat :: Natural -> Fin n
pattern FNat a <- Fin a

pattern FZero :: Fin n
pattern FZero = Fin 0

{-# COMPLETE FZero, FNat #-}
{-# COMPLETE FNat #-}

reduceFin :: Fin (n + 1) -> Fin n
reduceFin FZero = Fin 0
reduceFin (FNat n) = Fin (n - 1)
