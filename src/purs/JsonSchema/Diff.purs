module JsonSchema.Diff
  ( Difference
  , DifferenceType(..)
  , calculate
  , renderDifference
  ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import JsonSchema (JsonSchema(..), JsonValueType, Keywords)
import JsonSchema as Schema
import JsonSchema.SchemaPath (SchemaPath, SchemaPathSegment(..))
import JsonSchema.SchemaPath as SchemaPath

type Difference = { differenceType ∷ DifferenceType, path ∷ SchemaPath }

data DifferenceType
  = BooleanSchemaChange Boolean
  | ExclusiveMaximumChange (Maybe Number) (Maybe Number)
  | ExclusiveMinimumChange (Maybe Number) (Maybe Number)
  | MaximumChange (Maybe Number) (Maybe Number)
  | MinimumChange (Maybe Number) (Maybe Number)
  | MultipleOfChange (Maybe Number) (Maybe Number)
  | SchemaChangeFromBooleanToObject Boolean Keywords
  | SchemaChangeFromObjectToBoolean Keywords Boolean
  | TypeChange (Maybe (Set JsonValueType)) (Maybe (Set JsonValueType))

derive instance Eq DifferenceType
derive instance Generic DifferenceType _
derive instance Ord DifferenceType

instance Show DifferenceType where
  show = genericShow

calculate ∷ JsonSchema → JsonSchema → Set Difference
calculate = go Nil
  where
  go ∷ SchemaPath → JsonSchema → JsonSchema → Set Difference
  go path previousSchema nextSchema = case previousSchema, nextSchema of
    BooleanSchema previousBool, BooleanSchema nextBool →
      Set.empty
    BooleanSchema previousBool, ObjectSchema nextKeywords →
      Set.empty
    ObjectSchema previousKeywords, BooleanSchema nextBool →
      Set.empty
    ObjectSchema previousKeywords, ObjectSchema nextKeywords →
      calculateObjectSchemataDiff path previousKeywords nextKeywords

  calculateObjectSchemataDiff
    ∷ SchemaPath → Keywords → Keywords → Set Difference
  calculateObjectSchemataDiff path previousKeywords nextKeywords =
    foldMap
      (\f → f path previousKeywords nextKeywords)
      [ calculateMultipleOfDiff
      , calculateRangeDiff
      , calculateTypeKeywordDiff
      ]

calculateMultipleOfDiff
  ∷ ∀ r
  . SchemaPath
  → { multipleOf ∷ Maybe Number | r }
  → { multipleOf ∷ Maybe Number | r }
  → Set Difference
calculateMultipleOfDiff path previousKeywords nextKeywords =
  if previousKeywords.multipleOf == nextKeywords.multipleOf then
    Set.empty
  else Set.singleton
    { differenceType: MultipleOfChange
        previousKeywords.multipleOf
        nextKeywords.multipleOf
    , path: MultipleOf : path
    }

calculateRangeDiff ∷ SchemaPath → Keywords → Keywords → Set Difference
calculateRangeDiff path previousKeywords nextKeywords =
  exclusiveMaximumDiff
    <> exclusiveMinimumDiff
    <> maximumDiff
    <> minimumDiff
  where
  exclusiveMaximumDiff ∷ Set Difference
  exclusiveMaximumDiff =
    if
      previousKeywords.exclusiveMaximum == nextKeywords.exclusiveMaximum then
      Set.empty
    else Set.singleton
      { differenceType: ExclusiveMaximumChange
          previousKeywords.exclusiveMaximum
          nextKeywords.exclusiveMaximum
      , path: ExclusiveMaximum : path
      }

  exclusiveMinimumDiff ∷ Set Difference
  exclusiveMinimumDiff =
    if
      previousKeywords.exclusiveMinimum == nextKeywords.exclusiveMinimum then
      Set.empty
    else Set.singleton
      { differenceType: ExclusiveMinimumChange
          previousKeywords.exclusiveMinimum
          nextKeywords.exclusiveMinimum
      , path: ExclusiveMinimum : path
      }

  maximumDiff ∷ Set Difference
  maximumDiff =
    if
      previousKeywords.maximum == nextKeywords.maximum then
      Set.empty
    else Set.singleton
      { differenceType: MaximumChange
          previousKeywords.maximum
          nextKeywords.maximum
      , path: Maximum : path
      }

  minimumDiff ∷ Set Difference
  minimumDiff =
    if
      previousKeywords.minimum == nextKeywords.minimum then
      Set.empty
    else Set.singleton
      { differenceType: MinimumChange
          previousKeywords.minimum
          nextKeywords.minimum
      , path: Minimum : path
      }

calculateTypeKeywordDiff
  ∷ SchemaPath → Keywords → Keywords → Set Difference
calculateTypeKeywordDiff path previousKeywords nextKeywords =
  if previousKeywords.typeKeyword == nextKeywords.typeKeyword then
    Set.empty
  else
    Set.singleton
      { differenceType: TypeChange
          previousKeywords.typeKeyword
          nextKeywords.typeKeyword
      , path: TypeKeyword : path
      }

renderDifference ∷ Difference → Array String
renderDifference { differenceType, path } =
  [ "Schema path: " <> SchemaPath.render path ]
    <> renderDifferenceType
  where
  renderDifferenceType ∷ Array String
  renderDifferenceType = case differenceType of
    BooleanSchemaChange false →
      [ "Boolean schema changed to reject-all" ]
    BooleanSchemaChange true →
      [ "Boolean schema changed to allow-all" ]
    ExclusiveMaximumChange before after →
      [ "exclusiveMaximum changed from "
          <> renderOptionalNumber before
          <> "to"
          <> renderOptionalNumber after
      ]
    ExclusiveMinimumChange before after →
      [ "exclusiveMinimum changed from "
          <> renderOptionalNumber before
          <> "to"
          <> renderOptionalNumber after
      ]
    MaximumChange before after →
      [ "maximum changed from "
          <> renderOptionalNumber before
          <> "to"
          <> renderOptionalNumber after
      ]
    MinimumChange before after →
      [ "minimum changed from "
          <> renderOptionalNumber before
          <> "to"
          <> renderOptionalNumber after
      ]
    MultipleOfChange before after →
      [ "multipleOf changed from "
          <> renderOptionalNumber before
          <> " to "
          <> renderOptionalNumber after
      ]
    SchemaChangeFromBooleanToObject _ _ →
      [ "Boolean schema changed to object schema" ]
    SchemaChangeFromObjectToBoolean _ _ →
      [ "Object schema changed to boolean schema" ]
    TypeChange typesBefore typesAfter →
      [ "Change of accepted JSON value types from " ]
        <> renderJsonValueTypes typesBefore
        <> [ "to" ]
        <> renderJsonValueTypes typesAfter

  renderOptionalNumber ∷ Maybe Number → String
  renderOptionalNumber = maybe "unspecified" show

  renderJsonValueTypes ∷ Maybe (Set JsonValueType) → Array String
  renderJsonValueTypes = maybe
    [ "unspecified" ]
    ( foldMap
        ( \jsonValueType →
            [ "- " <> Schema.renderJsonValueType jsonValueType ]
        )
    )
