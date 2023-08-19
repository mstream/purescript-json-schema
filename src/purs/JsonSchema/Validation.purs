module JsonSchema.Validation
  ( Violation
  , ViolationReason(..)
  , renderViolation
  , validateAgainst
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List ((:))
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
import JsonSchema.JsonPath (JsonPath, JsonPathSegment(..))
import JsonSchema.JsonPath as JsonPath
import JsonSchema.SchemaPath (SchemaPath, SchemaPathSegment(..))
import JsonSchema.SchemaPath as SchemaPath

type Violation =
  { jsonPath ∷ JsonPath
  , reason ∷ ViolationReason
  , schemaPath ∷ SchemaPath
  }

data ViolationReason
  = AlwaysFailingSchema
  | InvalidArray (Set Violation)
  | TypeMismatch
      { actualJsonValueType ∷ JsonValueType
      , allowedJsonValueTypes ∷ Set JsonValueType
      }
  | ValidAgainstNotSchema

derive instance Eq ViolationReason
derive instance Generic ViolationReason _
derive instance Ord ViolationReason

instance Show ViolationReason where
  show reason = genericShow reason

renderViolation ∷ Violation → Array String
renderViolation { jsonPath, reason, schemaPath } =
  [ "Schema path:"
  , SchemaPath.render schemaPath
  , "JSON path:"
  , JsonPath.render jsonPath
  ]
    <> renderViolationReason reason

renderViolationReason ∷ ViolationReason → Array String
renderViolationReason = case _ of
  AlwaysFailingSchema →
    [ "Schema always fails validation." ]
  InvalidArray itemViolations →
    [ "Invalid array: " ] <> foldMap
      ( \violation →
          [ "-" ] <> (("  " <> _) <$> renderViolation violation)
      )
      itemViolations
  TypeMismatch { actualJsonValueType, allowedJsonValueTypes } →
    [ "Invalid type. Expected "
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
    ]
  ValidAgainstNotSchema →
    [ "JSON is valid against schema from 'not'." ]

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
    notViolations <> typeKeywordViolations <> A.caseJson
      (const Set.empty)
      (const Set.empty)
      (const Set.empty)
      (const Set.empty)
      ( \array →
          let
            shouldValidate = case keywords.typeKeyword of
              Just typeKeyword →
                JsonArray `Set.member` typeKeyword
              Nothing →
                true
          in
            if shouldValidate then
              let
                violations = validateArray schemaPath jsonPath array
                  keywords
              in
                if Set.isEmpty violations then Set.empty
                else
                  Set.singleton
                    { jsonPath
                    , reason: InvalidArray violations
                    , schemaPath
                    }

            else Set.empty
      )
      (const Set.empty)
      json
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

  validateArray
    ∷ ∀ r
    . SchemaPath
    → JsonPath
    → Array Json
    → { items ∷ Maybe JsonSchema | r }
    → Set Violation
  validateArray schemaPath jsonPath array constraints =
    itemsViolations
    where
    itemsViolations ∷ Set Violation
    itemsViolations = maybe
      Set.empty
      (validateItems (Items : schemaPath) jsonPath array)
      constraints.items

  validateItems
    ∷ SchemaPath → JsonPath → Array Json → JsonSchema → Set Violation
  validateItems schemaPath jsonPath itemJsons schema =
    foldMapWithIndex f itemJsons
    where
    f ∷ Int → Json → Set Violation
    f itemIndex itemJson = go
      schemaPath
      (ItemIndex itemIndex : jsonPath)
      itemJson
      schema

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
