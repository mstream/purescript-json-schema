module JsonSchema.Compatibility
  ( BackwardIncompatibility(..)
  , BackwardIncompatibilityType(..)
  , Compatibility(..)
  , ForwardIncompatibility(..)
  , ForwardIncompatibilityType(..)
  , NumberRangeChange(..)
  , calculate
  ) where

import Prelude

import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Argonaut.Encode as AE
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Markdown (PhrasingContentNode)
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.NonEmpty as NE
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as SetNE
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty as StringNE
import Docs.Document (class Document, document)
import JsonSchema (JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.Difference (Difference(..), DifferenceType(..))
import JsonSchema.Range (Boundary(..), Range)
import JsonSchema.Range as Range
import JsonSchema.SchemaPath (SchemaPath)
import JsonSchema.SchemaPath as SchemaPath
import Show.NonEmpty (show1)
import Type.Proxy (Proxy(..))
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

instance EncodeJson Compatibility where
  encodeJson = case _ of
    Backward details →
      "compatibilityType" := A.fromString "backward"
        ~> "incompabilities"
          :=
            ( "forward"
                :=
                  ( AE.encodeJson $ SetNE.toSet
                      details.forwardIncompatibilities
                  )
                ~> A.jsonEmptyString
            )
        ~> A.jsonEmptyObject
    Forward details →
      "compatibilityType" := A.fromString "forward"
        ~> "incompabilities"
          :=
            ( "backward"
                :=
                  ( AE.encodeJson $ SetNE.toSet
                      details.backwardIncompatibilities
                  )
                ~> A.jsonEmptyString
            )
        ~> A.jsonEmptyObject
    Full →
      A.jsonEmptyObject
    None details →
      "compatibilityType" := A.fromString "none"
        ~> "incompabilities"
          :=
            ( "backward"
                :=
                  ( AE.encodeJson $ SetNE.toSet
                      details.backwardIncompatibilities
                  )
                ~> "forward"
                  :=
                    ( AE.encodeJson $ SetNE.toSet
                        details.forwardIncompatibilities
                    )
                ~> A.jsonEmptyString
            )
        ~> A.jsonEmptyObject

instance Show Compatibility where
  show = genericShow

instance Document Compatibility where
  document = case _ of
    Backward { forwardIncompatibilities } →
      ( M.paragraph $ ArrayNE.singleton $ M.text $ StringNE.nes
          ( Proxy @"Reasons for breaking the forward compatibility:"
          )
      ) :|
        [ M.unorderedList $ ArrayNE.singleton
            <$>
              ( ArrayNE.fromFoldable1
                  $ document forwardIncompatibilities
              )
        ]
    Forward { backwardIncompatibilities } →
      ( M.paragraph $ ArrayNE.singleton $ M.text $
          StringNE.nes
            ( Proxy @"Reasons for breaking the backward compatibility:"
            )
      ) :|
        [ M.unorderedList $ ArrayNE.singleton
            <$>
              ( ArrayNE.fromFoldable1
                  $ document backwardIncompatibilities
              )
        ]
    Full →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes (Proxy @"✓")
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

derive newtype instance EncodeJson BackwardIncompatibility
derive newtype instance Eq BackwardIncompatibility
derive newtype instance Ord BackwardIncompatibility
derive newtype instance Show BackwardIncompatibility

instance Document BackwardIncompatibility where
  document (BackwardIncompatibility { incompatibilityType, path }) =
    ( M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes (Proxy @"schema path: ")
            <> SchemaPath.render path
    )
      :| (Array.fromFoldable $ document incompatibilityType)

newtype ForwardIncompatibility = ForwardIncompatibility
  { incompatibilityType ∷ ForwardIncompatibilityType
  , path ∷ SchemaPath
  }

derive newtype instance EncodeJson ForwardIncompatibility
derive newtype instance Eq ForwardIncompatibility
derive newtype instance Ord ForwardIncompatibility
derive newtype instance Show ForwardIncompatibility

instance Document ForwardIncompatibility where
  document (ForwardIncompatibility { incompatibilityType, path }) =
    ( M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes (Proxy @"schema path: ")
            <> SchemaPath.render path
    )
      :| Array.fromFoldable (document incompatibilityType)

data BackwardIncompatibilityType
  = MultipleIntroduced Number
  | OldMultipleIsNotFactorOfNewMultiple { new ∷ Number, old ∷ Number }
  | RangeOfAllowedNumbersReduced NumberRangeChange
  | SetOfAllowedTypesReduced
      (NonEmptySet JsonValueType)

derive instance Eq BackwardIncompatibilityType
derive instance Generic BackwardIncompatibilityType _
derive instance Ord BackwardIncompatibilityType

instance EncodeJson BackwardIncompatibilityType where
  encodeJson = case _ of
    MultipleIntroduced value →
      "multipleIntroduced" := A.fromNumber value
        ~> A.jsonEmptyObject
    OldMultipleIsNotFactorOfNewMultiple { new, old } →
      "oldMultipleIsNotFactorOfNewMultiple" := AE.encodeJson
        { new, old }
        ~> A.jsonEmptyObject
    RangeOfAllowedNumbersReduced removedRange →
      "rangeOfAllowedNumbersReduced" := AE.encodeJson removedRange
        ~> A.jsonEmptyObject
    SetOfAllowedTypesReduced removedTypes →
      "setOfAllowedTypesReduced" :=
        (AE.encodeJson $ SetNE.toSet removedTypes)
        ~> A.jsonEmptyObject

instance Show BackwardIncompatibilityType where
  show = genericShow

instance Document BackwardIncompatibilityType where
  document = NE.singleton <<< M.paragraph <<<
    case _ of
      MultipleIntroduced multiple →
        ( M.text $
            StringNE.nes
              (Proxy @"numerical values must be multiples of ")
        )
          `ArrayNE.cons'`
            [ M.text $ show1 multiple
            , M.text $ StringNE.nes (Proxy @" now")
            ]
      OldMultipleIsNotFactorOfNewMultiple { new, old } →
        ( M.text $
            StringNE.nes
              (Proxy @"the old multiple constraint of ")
        )
          `ArrayNE.cons'`
            [ M.text $ show1 old
            , M.text $ StringNE.nes
                ( Proxy
                    ∷ Proxy
                        " is not a factor of the new multiple constraint of "
                )
            , M.text $ show1 new
            ]
      RangeOfAllowedNumbersReduced change →
        ( M.text $ StringNE.nes
            ( Proxy
                ∷ Proxy
                    "the range of allowed values has been reduced by "
            )
        )
          `ArrayNE.cons` renderNumberRangeChange change
      SetOfAllowedTypesReduced removedTypes →
        ( M.text $ StringNE.nes
            ( Proxy
                ∷ Proxy
                    "the set of allowed JSON value types has been reduced by "
            )
        )
          `ArrayNE.cons'`
            [ M.text
                $ StringNE.join1With ", "
                $ Schema.renderJsonValueType
                    <$> ArrayNE.fromFoldable1 removedTypes
            ]

data ForwardIncompatibilityType
  = MultipleWithdrawn Number
  | NewMultipleIsNotFactorOfOldMultiple { new ∷ Number, old ∷ Number }
  | RangeOfAllowedNumbersExtended NumberRangeChange
  | SetOfAllowedTypesExtended
      (NonEmptySet JsonValueType)

derive instance Eq ForwardIncompatibilityType
derive instance Generic ForwardIncompatibilityType _
derive instance Ord ForwardIncompatibilityType

instance EncodeJson ForwardIncompatibilityType where
  encodeJson = case _ of
    MultipleWithdrawn value →
      "multipleWithdrawn" := A.fromNumber value
        ~> A.jsonEmptyObject
    NewMultipleIsNotFactorOfOldMultiple { new, old } →
      "newMultipleIsNotFactorOfOldMultiple" := AE.encodeJson
        { new, old }
        ~> A.jsonEmptyObject
    RangeOfAllowedNumbersExtended addedRange →
      "rangeOfAllowedNumbersExtended" := AE.encodeJson addedRange
        ~> A.jsonEmptyObject
    SetOfAllowedTypesExtended addedTypes →
      "setOfAllowedTypesExtended" :=
        (AE.encodeJson $ SetNE.toSet addedTypes)
        ~> A.jsonEmptyObject

instance Show ForwardIncompatibilityType where
  show = genericShow

instance Document ForwardIncompatibilityType where
  document = NE.singleton <<< M.paragraph <<<
    case _ of
      MultipleWithdrawn multiple →
        ( M.text $ StringNE.nes
            (Proxy @"numerical values must not be multiples of ")
        )
          `ArrayNE.cons'`
            [ M.text $ show1 multiple
            , M.text $ StringNE.nes (Proxy @"anymore")
            ]
      NewMultipleIsNotFactorOfOldMultiple { new, old } →
        ( M.text $ StringNE.nes
            (Proxy @"the new multiple constraint of ")
        )
          `ArrayNE.cons'`
            [ M.text $ show1 new
            , M.text $ StringNE.nes
                ( Proxy
                    ∷ Proxy
                        " is not a factor of the old multiple constraint of "
                )
            , M.text $ show1 old
            ]
      RangeOfAllowedNumbersExtended change →
        ( M.text $ StringNE.nes
            ( Proxy
                ∷ Proxy
                    "the range of allowed values has been extended by "
            )
        )
          `ArrayNE.cons` renderNumberRangeChange change
      SetOfAllowedTypesExtended removedTypes →
        ( M.text $ StringNE.nes
            ( Proxy
                ∷ Proxy
                    "the set of allowed JSON value types has been extended by "
            )
        )
          `ArrayNE.cons'`
            [ M.text
                $ StringNE.join1With ", "
                $ Schema.renderJsonValueType
                    <$> ArrayNE.fromFoldable1 removedTypes
            ]

data NumberRangeChange
  = Lower Range
  | LowerAndUpper Range Range
  | Upper Range

derive instance Eq NumberRangeChange
derive instance Generic NumberRangeChange _
derive instance Ord NumberRangeChange

instance EncodeJson NumberRangeChange where
  encodeJson = case _ of
    Lower range →
      "lower" := AE.encodeJson range
        ~> A.jsonEmptyObject
    LowerAndUpper lowerRange upperRange →
      "lower" := AE.encodeJson lowerRange
        ~> "upper" := AE.encodeJson upperRange
        ~> A.jsonEmptyObject
    Upper range →
      "upper" := AE.encodeJson range
        ~> A.jsonEmptyObject

instance Show NumberRangeChange where
  show = genericShow

renderNumberRangeChange
  ∷ NumberRangeChange → NonEmptyArray PhrasingContentNode
renderNumberRangeChange = case _ of
  Lower lower →
    ArrayNE.singleton $ Range.renderRange lower
  LowerAndUpper lower upper →
    Range.renderRange lower
      `ArrayNE.cons'`
        [ M.text $ StringNE.nes (Proxy @" and ")
        , Range.renderRange upper
        ]
  Upper upper →
    ArrayNE.singleton $ Range.renderRange upper

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
      case mbExtensionLowerRange, mbExtensionUpperRange of
        Nothing, Nothing →
          Full
        Just lower, Nothing →
          Backward
            { forwardIncompatibilities: SetNE.singleton $
                ForwardIncompatibility
                  { incompatibilityType: RangeOfAllowedNumbersExtended $
                      Lower lower
                  , path: Nil
                  }
            }
        Just lower, Just upper →
          Backward
            { forwardIncompatibilities: SetNE.singleton $
                ForwardIncompatibility
                  { incompatibilityType: RangeOfAllowedNumbersExtended $
                      LowerAndUpper lower upper
                  , path: Nil
                  }
            }
        Nothing, Just upper →
          Backward
            { forwardIncompatibilities: SetNE.singleton $
                ForwardIncompatibility
                  { incompatibilityType: RangeOfAllowedNumbersExtended $
                      Upper upper
                  , path: Nil
                  }
            }
    rangeReductionCompatibility =
      case mbReductionLowerRange, mbReductionUpperRange of
        Nothing, Nothing →
          Full
        Just lower, Nothing →
          Forward
            { backwardIncompatibilities: SetNE.singleton $
                BackwardIncompatibility
                  { incompatibilityType: RangeOfAllowedNumbersReduced $
                      Lower lower
                  , path: Nil
                  }
            }
        Just lower, Just upper →
          Forward
            { backwardIncompatibilities: SetNE.singleton $
                BackwardIncompatibility
                  { incompatibilityType: RangeOfAllowedNumbersReduced $
                      LowerAndUpper lower upper
                  , path: Nil
                  }
            }
        Nothing, Just upper →
          Forward
            { backwardIncompatibilities: SetNE.singleton $
                BackwardIncompatibility
                  { incompatibilityType: RangeOfAllowedNumbersReduced $
                      Upper upper
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
    typesAddedCompatibility = case SetNE.fromSet typesAdded of
      Nothing →
        Full
      Just types →
        Backward
          { forwardIncompatibilities: SetNE.singleton
              $ ForwardIncompatibility
                  { incompatibilityType:
                      SetOfAllowedTypesExtended types
                  , path: Nil
                  }
          }
    typesRemovedCompatibility = case SetNE.fromSet typesRemoved of
      Nothing →
        Full
      Just types →
        Forward
          { backwardIncompatibilities: SetNE.singleton
              $ BackwardIncompatibility
                  { incompatibilityType:
                      SetOfAllowedTypesReduced types
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
