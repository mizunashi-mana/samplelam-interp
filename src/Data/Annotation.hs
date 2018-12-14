module Data.Annotation where

import           Control.Lens          (view)
import           Data.Extensible       hiding (nil, (<:))
import           Data.Extensible.HList
import           Data.Profunctor       (dimap)
import           GHC.OverloadedLabels


newtype RevApply a f = RevApply
  { getRevApply :: f a
  }

instance Wrapper (RevApply i) where
  type Repr (RevApply i) f = f i
  _Wrapper = dimap getRevApply (fmap RevApply)

newtype Ann fs i = AnnRecord
  { getAnnRecord :: RecordOf (RevApply i) fs
  }

annBuild :: HList (Field (RevApply i)) fs -> Ann fs i
annBuild = AnnRecord . fromHList

(<:) :: h x -> HList h xs -> HList h (x ': xs)
(<:) = HCons

infixr 0 <:

nil :: HList h '[]
nil = HNil


newtype AnnFieldGetter fs f = AnnFieldGetter
  { getField :: Ann fs :~> f
  }
instance Associate k f fs => IsLabel k (AnnFieldGetter fs f) where
  fromLabel = AnnFieldGetter
    $ Nat (view (itemAssoc (Proxy :: Proxy k)) . getAnnRecord)
