module JsonSchema.Compatibility
  ( BackwardIncompatibility(..)
  , BackwardIncompatibilityType(..)
  , Compatibility(..)
  , ForwardIncompatibility(..)
  , ForwardIncompatibilityType(..)
  , calculate
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as ArrayNE
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.NonEmpty as NE
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as SetNE
import Data.Show.Generic (genericShow)
import Data.String as String
import Docs.Document (class Document, document)
import JsonSchema (JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.Difference (Difference(..), DifferenceType(..))
import JsonSchema.Range (Boundary(..), Range)
import JsonSchema.Range as Range
import JsonSchema.SchemaPath (SchemaPath)
import JsonSchema.SchemaPath as SchemaPath
import Utils (isInteger)

data Compatibility
  = Backward
      { forwardIncompatibilities ∷ NonEmptySet ForwardIncompatibility }
  | Forward
      { backwardIncompatibilities ∷ NonEmptySet BackwardIncompatibility
      }
  | Full
  | None
      { backwardIncompatibilities ∷ NonEmptySet BackwardIncompatibility
      , forwardIncompatibilities ∷ NonEmptySet ForwardIncompatibility
      }

derive instance Eq Compatibility
derive instance Generic Compatibility _

instance Show Compatibility where
  show = genericShow

instance Document Compatibility where
  document = case _ of
    Backward { forwardIncompatibilities } →
      ( M.paragraph $ ArrayNE.singleton $ M.text
          "Reasons for breaking the forward compatibility:"
      ) :|
        [ M.unorderedList $ ArrayNE.singleton
            <$>
              ( ArrayNE.fromFoldable1
                  $ document forwardIncompatibilities
              )
        ]
    Forward { backwardIncompatibilities } →
      ( M.paragraph $ ArrayNE.singleton $ M.text
          "Reasons for breaking the backward compatibility:"
      ) :|
        [ M.unorderedList $ ArrayNE.singleton
            <$>
              ( ArrayNE.fromFoldable1
                  $ document backwardIncompatibilities
              )
        ]
    Full →
      NE.singleton $ M.paragraph $ ArrayNE.singleton $ M.text "✓"
    None
      { backwardIncompatibilities
      , forwardIncompatibilities
      } →
      document (Backward { forwardIncompatibilities })
        <> document (Forward { backwardIncompatibilities })

newtype BackwardIncompatibility = BackwardIncompatibility
  { incompatibilityType ∷ BackwardIncompatibilityType
  , path ∷ SchemaPath
  }

derive newtype instance Eq BackwardIncompatibility
derive newtype instance Ord BackwardIncompatibility
derive newtype instance Show BackwardIncompatibility

instance Document BackwardIncompatibility where
  document (BackwardIncompatibility { incompatibilityType, path }) =
    ( M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ "schema path: " <> SchemaPath.render path
    )
      :| (Array.fromFoldable $ document incompatibilityType)

newtype ForwardIncompatibility = ForwardIncompatibility
  { incompatibilityType ∷ ForwardIncompatibilityType
  , path ∷ SchemaPath
  }

derive newtype instance Eq ForwardIncompatibility
derive newtype instance Ord ForwardIncompatibility
derive newtype instance Show ForwardIncompatibility

instance Document ForwardIncompatibility where
  document (ForwardIncompatibility { incompatibilityType, path }) =
    ( M.paragraph $ ArrayNE.singleton $ M.text $
        "schema path: " <> SchemaPath.render path
    ) :| Array.fromFoldable (document incompatibilityType)

data BackwardIncompatibilityType
  = MultipleIntroduced Number
  | OldMultipleIsNotFactorOfNewMultiple { new ∷ Number, old ∷ Number }
  | RangeOfAllowedNumbersReduced
      { lower ∷ Maybe Range, upper ∷ Maybe Range }
  | SetOfAllowedTypesReduced
      (Set JsonValueType)

derive instance Eq BackwardIncompatibilityType
derive instance Generic BackwardIncompatibilityType _
derive instance Ord BackwardIncompatibilityType

instance Show BackwardIncompatibilityType where
  show = genericShow

instance Document BackwardIncompatibilityType where
  document = NE.singleton <<< M.paragraph <<< map M.text <<<
    case _ of
      MultipleIntroduced multiple →
        ArrayNE.singleton "numerical values must be multiples of "
          <> ArrayNE.singleton (show multiple)
          <> ArrayNE.singleton "now"
      OldMultipleIsNotFactorOfNewMultiple { new, old } →
        ArrayNE.singleton "the old multiple constraint of "
          <> ArrayNE.singleton (show old)
          <> ArrayNE.singleton
            " is not a factor of the new multiple constraint of "
          <> ArrayNE.singleton (show new)
      RangeOfAllowedNumbersReduced { lower, upper } →
        ArrayNE.singleton
          "the range of allowed values has been reduced by"
          <> ArrayNE.singleton case lower, upper of
            Just l, Just u →
              " "
                <> Range.renderRange l
                <> " and "
                <> Range.renderRange u
            Just l, Nothing →
              " " <> Range.renderRange l
            Nothing, Just u →
              " " <> Range.renderRange u
            Nothing, Nothing →
              "FIXME: should never happen, inprove the data model"
      SetOfAllowedTypesReduced removedTypes →
        ArrayNE.singleton
          "the set of allowed JSON value types has been reduced by "
          <>
            ( ArrayNE.singleton $ String.joinWith ", " $
                Schema.renderJsonValueType <$> Array.fromFoldable
                  removedTypes
            )

data ForwardIncompatibilityType
  = MultipleWithdrawn Number
  | NewMultipleIsNotFactorOfOldMultiple { new ∷ Number, old ∷ Number }
  | RangeOfAllowedNumbersExtended
      { lower ∷ Maybe Range, upper ∷ Maybe Range }
  | SetOfAllowedTypesExtended
      (Set JsonValueType)

derive instance Eq ForwardIncompatibilityType
derive instance Generic ForwardIncompatibilityType _
derive instance Ord ForwardIncompatibilityType

instance Show ForwardIncompatibilityType where
  show = genericShow

instance Document ForwardIncompatibilityType where
  document = NE.singleton <<< M.paragraph <<< map M.text <<<
    case _ of
      MultipleWithdrawn multiple →
        ArrayNE.singleton "numerical values must not be multiples of "
          <> ArrayNE.singleton (show multiple)
          <> ArrayNE.singleton " anymore"
      NewMultipleIsNotFactorOfOldMultiple { new, old } →
        ArrayNE.singleton "the new multiple constraint of "
          <> ArrayNE.singleton (show new)
          <> ArrayNE.singleton
            " is not a factor of the olf multiple constraint of "
          <> ArrayNE.singleton (show old)
      RangeOfAllowedNumbersExtended { lower, upper } →
        ArrayNE.singleton
          "the range of allowed values has been extended by"
          <> ArrayNE.singleton case lower, upper of
            Just l, Just u →
              " "
                <> Range.renderRange l
                <> " and "
                <> Range.renderRange u
            Just l, Nothing →
              " " <> Range.renderRange l
            Nothing, Just u →
              " " <> Range.renderRange u
            Nothing, Nothing →
              "FIXME: should never happen, inprove the data model"
      SetOfAllowedTypesExtended removedTypes →
        ArrayNE.singleton
          "the set of allowed JSON value types has been extended by "
          <>
            ( ArrayNE.singleton $ String.joinWith ", " $
                Schema.renderJsonValueType <$> Array.fromFoldable
                  removedTypes
            )

calculate ∷ Set Difference → Compatibility
calculate differences = foldl
  ( \acc (Difference { differenceType }) →
      mergeCompatibility acc $ f differenceType
  )
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
        { forwardIncompatibilities: SetNE.singleton
            $ ForwardIncompatibility
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
        { backwardIncompatibilities: SetNE.singleton
            $ BackwardIncompatibility
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
    ( \acc (Difference { differenceType }) → case differenceType of
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
    ( \acc (Difference { differenceType }) → case differenceType of
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
    ( \acc (Difference { differenceType }) → case differenceType of
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
    ( \acc (Difference { differenceType }) → case differenceType of
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
      { backwardIncompatibilities: SetNE.singleton
          $ BackwardIncompatibility
              { incompatibilityType:
                  OldMultipleIsNotFactorOfNewMultiple
                    { new: after, old: before }
              , path: Nil
              }
      }
    else if isInteger $ after / before then Backward
      { forwardIncompatibilities: SetNE.singleton
          $ ForwardIncompatibility
              { incompatibilityType:
                  NewMultipleIsNotFactorOfOldMultiple
                    { new: after, old: before }
              , path: Nil
              }
      }
    else None
      { backwardIncompatibilities:
          SetNE.singleton $ BackwardIncompatibility
            { incompatibilityType: OldMultipleIsNotFactorOfNewMultiple
                { new: after, old: before }
            , path: Nil
            }
      , forwardIncompatibilities: SetNE.singleton
          $ ForwardIncompatibility
              { incompatibilityType:
                  NewMultipleIsNotFactorOfOldMultiple
                    { new: after, old: before }
              , path: Nil
              }
      }
  Just before, Nothing →
    Backward
      { forwardIncompatibilities:
          SetNE.singleton $ ForwardIncompatibility
            { incompatibilityType: MultipleWithdrawn before, path: Nil }
      }
  Nothing, Just after →
    Forward
      { backwardIncompatibilities: SetNE.singleton $
          BackwardIncompatibility
            { incompatibilityType: MultipleIntroduced after, path: Nil }
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
        { forwardIncompatibilities: SetNE.singleton
            $ ForwardIncompatibility
                { incompatibilityType:
                    SetOfAllowedTypesExtended typesAdded
                , path: Nil
                }
        }
    typesRemovedCompatibility =
      if Set.isEmpty typesRemoved then Full
      else Forward
        { backwardIncompatibilities: SetNE.singleton
            $ BackwardIncompatibility
                { incompatibilityType:
                    SetOfAllowedTypesReduced typesRemoved
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
          `union`
            nr.backwardIncompatibilities
      , forwardIncompatibilities: nl.forwardIncompatibilities
          `union` nr.forwardIncompatibilities
      }
  None n, Full →
    None n
  where
  mergeBackwardIncompabilities
    ∷ ∀ rl rr
    . { backwardIncompatibilities ∷ NonEmptySet BackwardIncompatibility
      | rl
      }
    → { backwardIncompatibilities ∷ NonEmptySet BackwardIncompatibility
      | rr
      }
    → NonEmptySet BackwardIncompatibility
  mergeBackwardIncompabilities
    { backwardIncompatibilities: bl }
    { backwardIncompatibilities: br } =
    bl `union` br

  mergeForwardIncompabilities
    ∷ ∀ rl rr
    . { forwardIncompatibilities ∷ NonEmptySet ForwardIncompatibility
      | rl
      }
    → { forwardIncompatibilities ∷ NonEmptySet ForwardIncompatibility
      | rr
      }
    → NonEmptySet ForwardIncompatibility
  mergeForwardIncompabilities
    { forwardIncompatibilities: fl }
    { forwardIncompatibilities: fr } =
    fl `union` fr

union ∷ ∀ a. Ord a ⇒ NonEmptySet a → NonEmptySet a → NonEmptySet a
union sl sr = SetNE.toSet sl `SetNE.unionSet` sr
