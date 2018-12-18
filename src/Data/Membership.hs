module Data.Membership
  ( Membership
  , MemberCtx (..)
  , MemberCtxT (..)
  , leadership
  , compareMembership
  , impossibleMembership
  , here
  , navNext

  , Member (..)
  , remember

  , (:~:) (..)
  ) where

import           Data.Constraint
import           Data.Extensible.Internal


class MemberCtx c fs r i where
  toMemberCtx :: f r i -> Membership fs f -> Dict (c (f r i))

instance MemberCtx c '[] r i where
  toMemberCtx _ n = impossibleMembership n

instance (c (f r i), MemberCtx c fs r i) => MemberCtx c (f ': fs) r i where
  toMemberCtx x n = leadership n (\Refl -> Dict) (toMemberCtx x)


class MemberCtxT c t fs r i where
  toMemberCtxT :: t f r i -> Membership fs f -> Dict (c (t f r i))

instance MemberCtxT c t '[] r i where
  toMemberCtxT _ n = impossibleMembership n

instance (c (t f r i), MemberCtxT c t fs r i) => MemberCtxT c t (f ': fs) r i where
  toMemberCtxT x n = leadership n (\Refl -> Dict) (toMemberCtxT x)
