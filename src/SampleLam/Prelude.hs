module SampleLam.Prelude
  ( module Prelude
  , module Control.Category
  , module Data.Kind
  , module GHC.Generics
  , module GHC.OverloadedLabels
  ) where

import           Control.Category
import           Data.Kind
import           Prelude          hiding (id, (.))
import           GHC.Generics (Generic(..))
import GHC.OverloadedLabels (IsLabel(..))
