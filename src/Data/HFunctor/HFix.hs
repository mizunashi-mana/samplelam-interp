module Data.HFunctor.HFix where

import SampleLam.Prelude

import Data.Nat
import Data.HFunctor


newtype HFix f i = HIn
  { hout :: f (HFix f) i
  }

deriving instance (forall r j. (forall i. Eq (r i)) => Eq (f r j)) => Eq (HFix f k)
deriving instance (forall r j. (forall i. Eq (r i)) => Ord (f r j)) => Ord (HFix f k)
deriving instance (forall r j. (forall i. Show (r i)) => Show (f r j)) => Show (HFix f k)


hcata :: HFunctor f => f a :~> a -> HFix f :~> a
hcata f = go
  where
    go = f . hfmap go . Nat hout
