module JsonSchema.Compatibility
  ( Compatibility(..)
  , calculate
  , renderCompatibility
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import JsonSchema (JsonValueType(..))
import JsonSchema.Diff (Difference, DifferenceType(..))
import Utils (isInteger)

data Compatibility = Backward | Forward | Full | None

derive instance Eq Compatibility
derive instance Generic Compatibility _

instance Show Compatibility where
  show = genericShow

calculate ∷ Set Difference → Compatibility
calculate = foldl
  (\acc diff → mergeCompatibility acc $ f diff.differenceType)
  Full
  where
  f ∷ DifferenceType → Compatibility
  f = case _ of
    MultipleOfChange mbBefore mbAfter →
      calculateMultipleOfChange mbBefore mbAfter
    TypeChange mbTypesBefore mbTypesAfter →
      calculateTypeChange mbTypesBefore mbTypesAfter
    _ →
      None

calculateMultipleOfChange ∷ Maybe Number → Maybe Number → Compatibility
calculateMultipleOfChange = case _, _ of
  Just before, Just after →
    if isInteger $ before / after then Forward
    else if isInteger $ after / before then Backward
    else None
  _, Nothing →
    Backward
  Nothing, _ →
    Forward

calculateTypeChange
  ∷ Maybe (Set JsonValueType)
  → Maybe (Set JsonValueType)
  → Compatibility
calculateTypeChange = case _, _ of
  Just typesBefore, Just typesAfter →
    let
      typesAdded = typesAfter `Set.difference` typesBefore
      typesRemoved = typesBefore `Set.difference` typesAfter
    in
      case Set.size typesAdded, Set.size typesRemoved of
        0, 0 →
          Full
        0, _ →
          case Array.fromFoldable typesRemoved of
            [ JsonInteger ] →
              if JsonNumber `Set.member` typesBefore then
                Full
              else Forward
            _ →
              Forward
        _, 0 →
          case Array.fromFoldable typesAdded of
            [ JsonInteger ] →
              if JsonNumber `Set.member` typesBefore then
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
  _, Nothing →
    Full
  Nothing, _ →
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
