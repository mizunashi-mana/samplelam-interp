{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.HFunctor.Unsafe where

import Data.Coerce
import Data.HFunctor
import Unsafe.Coerce
import Data.Nat


-- | Efficient coerce for @HFunctor@
--
-- Notice:
-- If @f@ would not be satisfied @HFunctor@ law, this function is too dangerous.
--
hfmapCoerce :: (forall i. Coercible (a i) (b i), HFunctor f) => f a :~> f b
hfmapCoerce = Nat unsafeCoerce
