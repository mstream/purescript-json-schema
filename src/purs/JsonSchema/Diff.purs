module JsonSchema.Diff (Difference(..), calculate, renderDifference) where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import JsonSchema (JsonSchema)

data Difference = Difference

derive instance Eq Difference

instance Show Difference where
  show _ = ""

calculate ∷ JsonSchema → JsonSchema → Set Difference
calculate _ _ = Set.empty

renderDifference ∷ Difference → Array String
renderDifference _ = [ "<TODO>" ]
