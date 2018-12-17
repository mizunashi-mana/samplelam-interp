module SampleLam.Prelude
  ( module Prelude
  , module Data.Kind
  , module Control.Category
  , module Data.Coerce
  , module GHC.Generics
  , module GHC.OverloadedLabels
  , module GHC.Records
  ) where

import           Control.Category
import           Data.Coerce
import           Data.Kind
import           GHC.Generics         (Generic (..))
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.Records          (HasField (..))
import           Prelude              hiding (id, (.))
