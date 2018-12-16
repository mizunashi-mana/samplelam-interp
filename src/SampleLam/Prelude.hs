module SampleLam.Prelude
  ( module Prelude
  , module Control.Category
  , module Data.Kind
  , module GHC.Generics
  , module GHC.OverloadedLabels
  , module GHC.Records
  ) where

import           Control.Category
import           Data.Kind
import           GHC.Generics         (Generic (..))
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.Records          (HasField (..))
import           Prelude              hiding (id, (.))
