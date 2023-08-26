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
calculate differences = foldl
  (\acc diff → mergeCompatibility acc $ f diff.differenceType)
  (calculateRangeChange differences)
  differences
  where
  f ∷ DifferenceType → Compatibility
  f = case _ of
    MultipleOfChange mbBefore mbAfter →
      calculateMultipleOfChange mbBefore mbAfter
    TypeChange mbTypesBefore mbTypesAfter →
      calculateTypeChange mbTypesBefore mbTypesAfter
    _ →
      Full

calculateRangeChange ∷ Set Difference → Compatibility
calculateRangeChange differences =
  if rangeExtended && rangeReduced then None
  else if rangeExtended then Backward
  else if rangeReduced then Forward
  else Full
  where
  rangeExtended ∷ Boolean
  rangeExtended = foldl
    ( \acc difference →
        acc || case difference.differenceType of
          ExclusiveMaximumChange (Just _) Nothing →
            true
          ExclusiveMaximumChange (Just before) (Just after) →
            after > before
          ExclusiveMinimumChange (Just _) Nothing →
            true
          ExclusiveMinimumChange (Just before) (Just after) →
            after < before
          MaximumChange (Just _) Nothing →
            true
          MaximumChange (Just before) (Just after) →
            after > before
          MinimumChange (Just _) Nothing →
            true
          MinimumChange (Just before) (Just after) →
            after < before
          _ →
            false
    )
    false
    differences

  rangeReduced ∷ Boolean
  rangeReduced = foldl
    ( \acc difference →
        acc || case difference.differenceType of
          ExclusiveMaximumChange Nothing (Just _) →
            true
          ExclusiveMaximumChange (Just before) (Just after) →
            after < before
          ExclusiveMinimumChange Nothing (Just _) →
            true
          ExclusiveMinimumChange (Just before) (Just after) →
            after > before
          MaximumChange Nothing (Just _) →
            true
          MaximumChange (Just before) (Just after) →
            after < before
          MinimumChange Nothing (Just _) →
            true
          MinimumChange (Just before) (Just after) →
            after > before
          _ →
            false
    )
    false
    differences

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
  other, Full →
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
