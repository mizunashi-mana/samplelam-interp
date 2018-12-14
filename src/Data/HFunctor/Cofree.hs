module Data.HFunctor.Cofree where

import SampleLam.Prelude

import Data.Nat
import Data.HFunctor


data HCofreeF f a r i = HCofreeF (a i) (f r i)
  deriving (Eq, Ord, Show)

instance HFunctor f => HFunctor (HCofreeF f a) where
  hfmap f = Nat \(HCofreeF x m) -> HCofreeF x (hfmapLack f m)
