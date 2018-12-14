module Data.Nat where

import           SampleLam.Prelude


newtype (f :: k -> Type) :~> (g :: k -> Type) = Nat
  { unNat :: forall i. f i -> g i
  }

instance Category (:~>) where
  id = Nat id
  Nat f . Nat g = Nat (f . g)
