module JsonSchema.Diff
  ( Difference
  , DifferenceType(..)
  , calculate
  , renderDifference
  ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Maybe (Maybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import JsonSchema (JsonSchema(..), JsonValueType, Keywords)
import JsonSchema as Schema
import JsonSchema.SchemaPath (SchemaPath)
import JsonSchema.SchemaPath as SchemaPath

type Difference = { differenceType ∷ DifferenceType, path ∷ SchemaPath }

data DifferenceType
  = BooleanSchemaChange Boolean
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
    if previousKeywords.typeKeyword == nextKeywords.typeKeyword then
      Set.empty
    else
      Set.singleton
        { differenceType: TypeChange
            previousKeywords.typeKeyword
            nextKeywords.typeKeyword
        , path
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
    SchemaChangeFromBooleanToObject _ _ →
      [ "Boolean schema changed to object schema" ]
    SchemaChangeFromObjectToBoolean _ _ →
      [ "Object schema changed to boolean schema" ]
    TypeChange typesBefore typesAfter →
      [ "Change of accepted JSON value types from " ]
        <> renderJsonValueTypes typesBefore
        <> [ "to" ]
        <> renderJsonValueTypes typesAfter

  renderJsonValueTypes ∷ Maybe (Set JsonValueType) → Array String
  renderJsonValueTypes = maybe
    [ "unspecified" ]
    ( foldMap
        ( \jsonValueType →
            [ "- " <> Schema.renderJsonValueType jsonValueType ]
        )
    )
