module Data.HFunctor.OpenUnion where

import SampleLam.Prelude

import Data.Nat
import Data.HFunctor
import Data.Extensible.Internal
import Data.Constraint


data HUnion fs r i where
  HUnion :: Membership fs f -> f r i -> HUnion fs r i

injectU :: Member fs f => f r :~> HUnion fs r
injectU = Nat (HUnion membership)

injectHere :: f r :~> HUnion (f ': fs) r
injectHere = Nat (HUnion here)

decomp :: f r :~> a -> HUnion fs r :~> a -> HUnion (f ': fs) r :~> a
decomp (Nat f) (Nat g) = Nat \(HUnion n x) ->
  leadership n (\Refl -> f x) (\m -> g $ HUnion m x)

weaken :: HUnion fs r :~> HUnion (f ': fs) r
weaken = Nat \(HUnion n x) -> HUnion (navNext n) x

absurdU :: HUnion '[] r :~> a
absurdU = Nat \(HUnion n _) -> impossibleMembership n


instance HFunctor (HUnion '[]) where
  hfmap _ = absurdU

instance (HFunctor f, HFunctor (HUnion fs)) => HFunctor (HUnion (f ': fs)) where
  hfmap f = decomp (injectHere . hfmap f) (weaken . hfmap f)


class MemberCtx c fs r i where
  toMemberCtx :: f r i -> Membership fs f -> Dict (c (f r i))

instance MemberCtx c '[] r i where
  toMemberCtx _ n = impossibleMembership n

instance (c (f r i), MemberCtx c fs r i) => MemberCtx c (f ': fs) r i where
  toMemberCtx x n = leadership n (\Refl -> Dict) (toMemberCtx x)


instance MemberCtx Eq fs r i => Eq (HUnion fs r i) where
  HUnion n1 x1 == HUnion n2 x2 = case compareMembership n1 n2 of
    Left  _    -> False
    Right Refl -> case toMemberCtx @Eq x1 n1 of Dict -> x1 == x2

instance (MemberCtx Eq fs r i, MemberCtx Ord fs r i) => Ord (HUnion fs r i) where
  HUnion n1 x1 `compare` HUnion n2 x2 = case compareMembership n1 n2 of
    Left  r    -> r
    Right Refl -> case toMemberCtx @Ord x1 n1 of Dict -> x1 `compare` x2


data HUnionShow fs r i where
  HU :: Show (f r i) => Membership fs f -> f r i -> HUnionShow fs r i

deriving instance Show (HUnionShow fs r i)

toHUnionShow :: MemberCtx Show fs r i => HUnion fs r i -> HUnionShow fs r i
toHUnionShow (HUnion n x) = withDict (toMemberCtx @Show x n) HU n x

instance MemberCtx Show fs r i => Show (HUnion fs r i) where
  showsPrec i = showsPrec i . toHUnionShow
