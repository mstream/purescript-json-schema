module JsonSchema.Compatibility
  ( BackwardIncompatibility(..)
  , Compatibility(..)
  , ForwardIncompatibility(..)
  , Incompatibility
  , calculate
  , renderCompatibility
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import JsonSchema (JsonValueType(..))
import JsonSchema.Diff (Difference, DifferenceType(..))
import JsonSchema.Range (Boundary(..), Range)
import JsonSchema.SchemaPath (SchemaPath)
import Utils (isInteger)

data Compatibility
  = Backward
      { forwardIncompatibilities ∷
          Set (Incompatibility ForwardIncompatibility)
      }
  | Forward
      { backwardIncompatibilities ∷
          Set (Incompatibility BackwardIncompatibility)
      }
  | Full
  | None
      { backwardIncompatibilities ∷
          Set (Incompatibility BackwardIncompatibility)
      , forwardIncompatibilities ∷
          Set (Incompatibility ForwardIncompatibility)
      }

derive instance Eq Compatibility
derive instance Generic Compatibility _

instance Show Compatibility where
  show = genericShow

type Incompatibility a =
  { incompatibilityType ∷ a
  , path ∷ SchemaPath
  }

data BackwardIncompatibility
  = MultpipleIntroduced Number
  | OldMultipleIsNotFactorOfNewMultiple { new ∷ Number, old ∷ Number }
  | RangeOfAllowedNumbersReduced
      { lower ∷ Maybe Range, upper ∷ Maybe Range }
  | SetOfAllowedTypesReduced
      (Set JsonValueType)

derive instance Eq BackwardIncompatibility
derive instance Generic BackwardIncompatibility _
derive instance Ord BackwardIncompatibility

instance Show BackwardIncompatibility where
  show = genericShow

data ForwardIncompatibility
  = MultipleWithdrawn Number
  | NewMultipleIsNotFactorOfOldMultiple { new ∷ Number, old ∷ Number }
  | RangeOfAllowedNumbersExtended
      { lower ∷ Maybe Range, upper ∷ Maybe Range }
  | SetOfAllowedTypesExtended
      (Set JsonValueType)

derive instance Eq ForwardIncompatibility
derive instance Generic ForwardIncompatibility _
derive instance Ord ForwardIncompatibility

instance Show ForwardIncompatibility where
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
  let
    rangeExtensionCompatibility =
      if
        mbExtensionLowerRange == Nothing && mbExtensionUpperRange ==
          Nothing then Full
      else Backward
        { forwardIncompatibilities: Set.singleton
            { incompatibilityType: RangeOfAllowedNumbersExtended
                { lower: mbExtensionLowerRange
                , upper: mbExtensionUpperRange
                }
            , path: Nil
            }
        }
    rangeReductionCompatibility =
      if
        mbReductionLowerRange == Nothing && mbReductionUpperRange ==
          Nothing then Full
      else Forward
        { backwardIncompatibilities: Set.singleton
            { incompatibilityType: RangeOfAllowedNumbersReduced
                { lower: mbReductionLowerRange
                , upper: mbReductionUpperRange
                }
            , path: Nil
            }
        }
  in
    mergeCompatibility
      rangeExtensionCompatibility
      rangeReductionCompatibility
  where
  mbExtensionLowerRange ∷ Maybe Range
  mbExtensionLowerRange = foldl
    ( \acc { differenceType } → case differenceType of
        ExclusiveMinimumChange (Just before) Nothing →
          Just { from: Open bottom, to: Closed before }
        ExclusiveMinimumChange (Just before) (Just after) →
          if after < before then Just
            { from: Open after, to: Closed before }
          else acc
        MinimumChange (Just before) Nothing →
          Just { from: Open bottom, to: Closed before }
        MinimumChange (Just before) (Just after) →
          if after < before then Just
            { from: Closed after, to: Open before }
          else acc
        _ →
          acc
    )
    Nothing
    differences

  mbExtensionUpperRange ∷ Maybe Range
  mbExtensionUpperRange = foldl
    ( \acc { differenceType } → case differenceType of
        ExclusiveMaximumChange (Just before) Nothing →
          Just { from: Closed before, to: Open top }
        ExclusiveMaximumChange (Just before) (Just after) →
          if after > before then Just
            { from: Closed before, to: Open after }
          else acc
        MaximumChange (Just before) Nothing →
          Just { from: Open before, to: Open top }
        MaximumChange (Just before) (Just after) →
          if after > before then Just
            { from: Open before, to: Closed after }
          else acc
        _ →
          acc
    )
    Nothing
    differences

  mbReductionLowerRange ∷ Maybe Range
  mbReductionLowerRange = foldl
    ( \acc { differenceType } → case differenceType of
        ExclusiveMinimumChange Nothing (Just after) →
          Just { from: Open bottom, to: Open after }
        ExclusiveMinimumChange (Just before) (Just after) →
          if after > before then Just
            { from: Open before, to: Closed after }
          else acc
        MinimumChange Nothing (Just after) →
          Just { from: Open bottom, to: Open after }
        MinimumChange (Just before) (Just after) →
          if after > before then Just
            { from: Closed before, to: Open after }
          else acc
        _ →
          acc
    )
    Nothing
    differences

  mbReductionUpperRange ∷ Maybe Range
  mbReductionUpperRange = foldl
    ( \acc { differenceType } → case differenceType of
        ExclusiveMaximumChange Nothing (Just after) →
          Just { from: Open after, to: Open top }
        ExclusiveMaximumChange (Just before) (Just after) →
          if after < before then Just
            { from: Closed after, to: Open before }
          else acc
        MaximumChange Nothing (Just after) →
          Just { from: Open after, to: Open top }
        MaximumChange (Just before) (Just after) →
          if after < before then Just
            { from: Open after, to: Closed before }
          else acc
        _ →
          acc
    )
    Nothing
    differences

calculateMultipleOfChange ∷ Maybe Number → Maybe Number → Compatibility
calculateMultipleOfChange = case _, _ of
  Just before, Just after →
    if isInteger $ before / after then Forward
      { backwardIncompatibilities: Set.singleton
          { incompatibilityType: OldMultipleIsNotFactorOfNewMultiple
              { new: after, old: before }
          , path: Nil
          }
      }
    else if isInteger $ after / before then Backward
      { forwardIncompatibilities: Set.singleton
          { incompatibilityType:
              NewMultipleIsNotFactorOfOldMultiple
                { new: after, old: before }
          , path: Nil
          }
      }
    else None
      { backwardIncompatibilities:
          Set.singleton
            { incompatibilityType: OldMultipleIsNotFactorOfNewMultiple
                { new: after, old: before }
            , path: Nil
            }
      , forwardIncompatibilities: Set.singleton
          { incompatibilityType:
              NewMultipleIsNotFactorOfOldMultiple
                { new: after, old: before }
          , path: Nil
          }
      }
  Just before, Nothing →
    Backward
      { forwardIncompatibilities:
          Set.singleton
            { incompatibilityType: MultipleWithdrawn before, path: Nil }
      }
  Nothing, Just after →
    Forward
      { backwardIncompatibilities: Set.singleton
          { incompatibilityType: MultpipleIntroduced after, path: Nil }
      }
  Nothing, Nothing →
    Full

calculateTypeChange
  ∷ Maybe (Set JsonValueType)
  → Maybe (Set JsonValueType)
  → Compatibility
calculateTypeChange mbTypesBefore mbTypesAfter =
  let
    acceptedTypesBefore = effectiveTypes mbTypesBefore
    acceptedTypesAfter = effectiveTypes mbTypesAfter
    typesAdded =
      acceptedTypesAfter `Set.difference` acceptedTypesBefore
    typesRemoved =
      acceptedTypesBefore `Set.difference` acceptedTypesAfter
    typesAddedCompatibility =
      if Set.isEmpty typesAdded then Full
      else Backward
        { forwardIncompatibilities: Set.singleton
            { incompatibilityType: SetOfAllowedTypesExtended typesAdded
            , path: Nil
            }
        }
    typesRemovedCompatibility =
      if Set.isEmpty typesRemoved then Full
      else Forward
        { backwardIncompatibilities: Set.singleton
            { incompatibilityType: SetOfAllowedTypesReduced typesRemoved
            , path: Nil
            }
        }
  in
    mergeCompatibility typesAddedCompatibility typesRemovedCompatibility

effectiveTypes ∷ Maybe (Set JsonValueType) → Set JsonValueType
effectiveTypes = case _ of
  Just types →
    if JsonNumber `Set.member` types then
      JsonInteger `Set.insert` types
    else types
  Nothing →
    Set.fromFoldable
      [ JsonArray
      , JsonBoolean
      , JsonInteger
      , JsonNumber
      , JsonNull
      , JsonObject
      , JsonString
      ]

mergeCompatibility ∷ Compatibility → Compatibility → Compatibility
mergeCompatibility = case _, _ of
  Backward bl, Backward br →
    Backward
      { forwardIncompatibilities: mergeForwardIncompabilities bl br }
  Backward b, Forward f →
    None
      { backwardIncompatibilities: f.backwardIncompatibilities
      , forwardIncompatibilities: b.forwardIncompatibilities
      }
  Backward b, Full →
    Backward b
  Backward b, None n →
    None n
      { forwardIncompatibilities = mergeForwardIncompabilities b n
      }
  Forward f, Backward b →
    None
      { backwardIncompatibilities: f.backwardIncompatibilities
      , forwardIncompatibilities: b.forwardIncompatibilities
      }
  Forward fl, Forward fr →
    Forward
      { backwardIncompatibilities: mergeBackwardIncompabilities fl fr }
  Forward f, Full →
    Forward f
  Forward f, None n →
    None n
      { backwardIncompatibilities = mergeBackwardIncompabilities f n }
  Full, other →
    other
  None n, Backward b →
    None n
      { forwardIncompatibilities = mergeForwardIncompabilities b n
      }
  None n, Forward f →
    None n
      { backwardIncompatibilities = mergeBackwardIncompabilities f n }
  None nl, None nr →
    None
      { backwardIncompatibilities: nl.backwardIncompatibilities
          `Set.union`
            nr.backwardIncompatibilities
      , forwardIncompatibilities: nl.forwardIncompatibilities
          `Set.union` nr.forwardIncompatibilities
      }
  None n, Full →
    None n
  where
  mergeBackwardIncompabilities
    ∷ ∀ rl rr
    . { backwardIncompatibilities ∷
          Set (Incompatibility BackwardIncompatibility)
      | rl
      }
    → { backwardIncompatibilities ∷
          Set (Incompatibility BackwardIncompatibility)
      | rr
      }
    → Set (Incompatibility BackwardIncompatibility)
  mergeBackwardIncompabilities
    { backwardIncompatibilities: bl }
    { backwardIncompatibilities: br } =
    bl `Set.union` br

  mergeForwardIncompabilities
    ∷ ∀ rl rr
    . { forwardIncompatibilities ∷
          Set (Incompatibility ForwardIncompatibility)
      | rl
      }
    → { forwardIncompatibilities ∷
          Set (Incompatibility ForwardIncompatibility)
      | rr
      }
    → Set (Incompatibility ForwardIncompatibility)
  mergeForwardIncompabilities
    { forwardIncompatibilities: fl }
    { forwardIncompatibilities: fr } =
    fl `Set.union` fr

renderCompatibility ∷ Compatibility → String
renderCompatibility = case _ of
  Backward _ →
    "backward"
  Forward _ →
    "forward"
  Full →
    "full"
  None _ →
    "none"
