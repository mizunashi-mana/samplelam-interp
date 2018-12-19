module SampleLam.Prelude
  ( module Prelude
  , module Data.Kind
  , module Control.Category
  , module Data.Function
  , module Data.Foldable
  , module Data.Coerce
  , module GHC.Generics
  , module GHC.OverloadedLabels
  , module GHC.Records
  ) where

import           Control.Category
import           Data.Coerce
import           Data.Kind
import           Data.Function        hiding (id, (.))
import           GHC.Generics         (Generic (..))
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.Records          (HasField (..))
import           Prelude              hiding (id, (.), foldl)
import           Data.Foldable (foldl')
