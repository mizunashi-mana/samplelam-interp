module Data.HFunctor.Identity where

import Data.Nat
import Data.HFunctor


newtype HIdentity r i = HIdentity
  { getHIdentity :: r i
  }

instance HFunctor HIdentity where
  hfmap (Nat f) = Nat \(HIdentity m) -> HIdentity (f m)
