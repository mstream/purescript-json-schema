module JsonSchema.Compatibility
  ( Compatibility(..)
  , calculate
  , renderCompatibility
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import JsonSchema (JsonValueType(..))
import JsonSchema.Diff (Difference, DifferenceType(..))

data Compatibility = Backward | Forward | Full | None

derive instance Eq Compatibility
derive instance Generic Compatibility _

instance Show Compatibility where
  show = genericShow

calculate ∷ Set Difference → Compatibility
calculate differences =
  if Set.isEmpty differences then Full
  else foldl mergeCompatibility Full
    ((f <<< _.differenceType) <$> List.fromFoldable differences)
  where
  f ∷ DifferenceType → Compatibility
  f = case _ of
    TypeChange (Just previousTypes) (Just nextTypes) →
      let
        typesAdded = nextTypes `Set.difference` previousTypes
        typesRemoved = previousTypes `Set.difference` nextTypes
      in
        case Set.size typesAdded, Set.size typesRemoved of
          0, 0 →
            Full
          0, _ →
            case Array.fromFoldable typesRemoved of
              [ JsonInteger ] →
                if JsonNumber `Set.member` previousTypes then
                  Full
                else Forward
              _ →
                Forward
          _, 0 →
            case Array.fromFoldable typesAdded of
              [ JsonInteger ] →
                if JsonNumber `Set.member` previousTypes then
                  Full
                else Backward
              _ →
                Backward
          _, _ →
            case
              Array.fromFoldable typesAdded,
              Array.fromFoldable typesRemoved
              of
              [ JsonInteger ], [ JsonNumber ] →
                Forward
              [ JsonNumber ], [ JsonInteger ] →
                Backward
              _, _ →
                None
    TypeChange _ Nothing →
      Full
    TypeChange Nothing _ →
      None
    _ →
      None

mergeCompatibility ∷ Compatibility → Compatibility → Compatibility
mergeCompatibility = case _, _ of
  Backward, Backward →
    Backward
  Forward, Forward →
    Forward
  Full, other →
    other
  _, _ →
    None

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
