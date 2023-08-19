module JsonSchema.Compatibility
  ( Compatibility(..)
  , calculate
  , renderCompatibility
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import JsonSchema.Diff (Difference)

data Compatibility = Backward | Forward | Full | None

derive instance Eq Compatibility
derive instance Generic Compatibility _

instance Show Compatibility where
  show = genericShow

calculate ∷ Set Difference → Compatibility
calculate differences = if Set.isEmpty differences then Full else None

renderCompatibility ∷ Compatibility → String
renderCompatibility = case _ of
  Backward →
    "backward"
  Forward →
    "forward"
  Full →
    "full"
  None →
    "none"
