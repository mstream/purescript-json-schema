module JsonSchema.Validation
  ( JsonPath
  , JsonPathSegment(..)
  , SchemaPath
  , SchemaPathSegment(..)
  , Violation
  , ViolationReason(..)
  , renderJsonPath
  , renderSchemaPath
  , renderViolationReason
  , validateAgainst
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import JsonSchema
  ( JsonSchema(..)
  , JsonValueType(..)
  , Keywords
  , renderJsonValueType
  )
import JsonSchema as Schema

type JsonPath = List JsonPathSegment

renderJsonPath ∷ JsonPath → String
renderJsonPath = ("$" <> _) <<< foldMap f <<< List.reverse
  where
  f ∷ JsonPathSegment → String
  f = case _ of
    Property name →
      "/" <> name

data JsonPathSegment = Property String

derive instance Eq JsonPathSegment
derive instance Generic JsonPathSegment _
derive instance Ord JsonPathSegment

instance Show JsonPathSegment where
  show = genericShow

type SchemaPath = List SchemaPathSegment

renderSchemaPath ∷ SchemaPath → String
renderSchemaPath = ("#" <> _) <<< foldMap f <<< List.reverse
  where
  f ∷ SchemaPathSegment → String
  f = case _ of
    TypeKeyword →
      "/type"

data SchemaPathSegment = TypeKeyword

derive instance Eq SchemaPathSegment
derive instance Generic SchemaPathSegment _
derive instance Ord SchemaPathSegment

instance Show SchemaPathSegment where
  show = genericShow

type Violation =
  { jsonPath ∷ JsonPath
  , reason ∷ ViolationReason
  , schemaPath ∷ SchemaPath
  }

data ViolationReason
  = AlwaysFailingSchema
  | TypeMismatch
      { actualJsonValueType ∷ JsonValueType
      , allowedJsonValueTypes ∷ Set JsonValueType
      }
  | ValidAgainstNotSchema

derive instance Eq ViolationReason
derive instance Generic ViolationReason _
derive instance Ord ViolationReason

instance Show ViolationReason where
  show = genericShow

renderViolationReason ∷ ViolationReason → String
renderViolationReason = case _ of
  AlwaysFailingSchema →
    "Schema always fails validation."
  TypeMismatch { actualJsonValueType, allowedJsonValueTypes } →
    "Invalid type. Expected "
      <>
        ( case Array.fromFoldable allowedJsonValueTypes of
            [] →
              "none"
            [ allowedJsonValueType ] →
              Schema.renderJsonValueType allowedJsonValueType
            _ →
              String.joinWith " or "
                $ renderJsonValueType
                    <$> Array.fromFoldable allowedJsonValueTypes
        )
      <> " but got "
      <> Schema.renderJsonValueType actualJsonValueType
      <> "."
  ValidAgainstNotSchema →
    "JSON is valid against schema from 'not'."

validateAgainst ∷ Json → JsonSchema → Set Violation
validateAgainst = go mempty mempty
  where
  go ∷ SchemaPath → JsonPath → Json → JsonSchema → Set Violation
  go schemaPath jsonPath json schema = case schema of
    BooleanSchema bool →
      if bool then Set.empty
      else Set.singleton
        { jsonPath, reason: AlwaysFailingSchema, schemaPath }

    ObjectSchema keywords →
      validateAgainstObjectSchema schemaPath jsonPath json keywords

validateAgainstObjectSchema
  ∷ SchemaPath → JsonPath → Json → Keywords → Set Violation
validateAgainstObjectSchema schemaPath jsonPath json keywords =
  notViolations <> typeKeywordViolations
  where
  notViolations ∷ Set Violation
  notViolations = case keywords.not of
    Just schema →
      if Set.isEmpty $ validateAgainst json schema then Set.singleton
        { jsonPath
        , reason: ValidAgainstNotSchema
        , schemaPath
        }
      else Set.empty
    Nothing →
      Set.empty

  typeKeywordViolations ∷ Set Violation
  typeKeywordViolations = maybe
    Set.empty
    (validateTypeKeyword schemaPath jsonPath json)
    keywords.typeKeyword

validateTypeKeyword
  ∷ SchemaPath → JsonPath → Json → Set JsonValueType → Set Violation
validateTypeKeyword schemaPath jsonPath json allowedJsonValueTypes =
  if jsonValueType `Set.member` allowedJsonValueTypes then Set.empty
  else Set.singleton
    { jsonPath
    , reason: TypeMismatch
        { actualJsonValueType: jsonValueType, allowedJsonValueTypes }
    , schemaPath: TypeKeyword : schemaPath
    }
  where
  jsonValueType ∷ JsonValueType
  jsonValueType = A.caseJson
    (const JsonNull)
    (const JsonBoolean)
    ( \x →
        if (Int.toNumber $ Int.trunc x) == x then JsonInteger
        else JsonNumber
    )
    (const JsonString)
    (const JsonArray)
    (const JsonObject)
    json
