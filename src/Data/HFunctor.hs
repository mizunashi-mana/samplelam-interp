module Data.HFunctor where

import           SampleLam.Prelude

import           Data.Functor.Compose
import           Data.Nat


-- | Higher order functor
--
-- > hfmap id = id
-- > hfmap f . hfmap g = hfmap (f . g)
--
class HFunctor f where
  hfmap :: (a :~> b) -> (f a :~> f b)

hfmapLack :: HFunctor f => a :~> b -> f a i -> f b i
hfmapLack f = unNat (hfmap f)


instance Functor f => HFunctor (Compose f) where
  hfmap (Nat f) = Nat (Compose . fmap f . getCompose)
