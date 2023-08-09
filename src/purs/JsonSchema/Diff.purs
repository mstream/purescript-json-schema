module JsonSchema.Diff where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import JsonSchema (JsonSchema)

data Difference = Difference

derive instance Eq Difference

instance Show Difference where
  show _ = ""

calculate ∷ JsonSchema → JsonSchema → Maybe (Set Difference)
calculate _ _ = Nothing
