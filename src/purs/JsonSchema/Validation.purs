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
import Data.Foldable (foldMap, foldl)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
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
import JsonSchema.Range (Boundary(..), Range)
import JsonSchema.Range as Range
import JsonSchema.SchemaPath (SchemaPath, SchemaPathSegment(..))
import JsonSchema.SchemaPath as SchemaPath
import Utils (isInteger)

type Violation =
  { jsonPath ∷ JsonPath
  , reason ∷ ViolationReason
  , schemaPath ∷ SchemaPath
  }

data ViolationReason
  = AlwaysFailingSchema
  | InvalidArray (Set Violation)
  | InvalidMultiple { expectedMultiple ∷ Number, value ∷ Number }
  | InvalidRange { validRange ∷ Range, value ∷ Number }
  | NonUniqueArrayItem
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
  [ "Schema path: " <> SchemaPath.render schemaPath
  , "JSON path: " <> JsonPath.render jsonPath
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
  InvalidMultiple { expectedMultiple, value } →
    [ show value <> " is not a multiple of " <> show expectedMultiple ]
  InvalidRange { validRange, value } →
    [ show value
        <> " is outside of the valid range of "
        <> Range.renderRange validRange
    ]
  NonUniqueArrayItem →
    [ "Non-unique array item." ]
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
validateAgainst = go Nil Nil
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
      (validateNumber schemaPath jsonPath keywords)
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
    → { items ∷ Maybe JsonSchema, uniqueItems ∷ Boolean | r }
    → Set Violation
  validateArray schemaPath jsonPath array constraints =
    itemsViolations <> uniqueItemsViolations
    where
    itemsViolations ∷ Set Violation
    itemsViolations = maybe
      Set.empty
      (validateItems (Items : schemaPath) jsonPath array)
      constraints.items

    uniqueItemsViolations ∷ Set Violation
    uniqueItemsViolations =
      if constraints.uniqueItems then
        validateUniqueItems (UniqueItems : schemaPath) jsonPath array
      else Set.empty

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

validateNumber
  ∷ ∀ r
  . SchemaPath
  → JsonPath
  → { exclusiveMaximum ∷ Maybe Number
    , exclusiveMinimum ∷ Maybe Number
    , maximum ∷ Maybe Number
    , minimum ∷ Maybe Number
    , multipleOf ∷ Maybe Number
    | r
    }
  → Number
  → Set Violation
validateNumber schemaPath jsonPath constraints x =
  validateMultipleOf schemaPath jsonPath x constraints.multipleOf
    <> rangeViolations
  where
  rangeViolations ∷ Set Violation
  rangeViolations = exclusiveMaximumViolations
    <> exclusiveMinimumViolations
    <> maximumViolations
    <> minimumViolations

  exclusiveMaximumViolations ∷ Set Violation
  exclusiveMaximumViolations = maybe
    Set.empty
    ( \exclusiveMaximum →
        if x < exclusiveMaximum then Set.empty
        else Set.singleton
          { jsonPath
          , reason: InvalidRange { validRange, value: x }
          , schemaPath: ExclusiveMaximum : schemaPath
          }
    )
    constraints.exclusiveMaximum

  exclusiveMinimumViolations ∷ Set Violation
  exclusiveMinimumViolations = maybe
    Set.empty
    ( \exclusiveMinimum →
        if x > exclusiveMinimum then Set.empty
        else Set.singleton
          { jsonPath
          , reason: InvalidRange { validRange, value: x }
          , schemaPath: ExclusiveMinimum : schemaPath
          }
    )
    constraints.exclusiveMinimum

  maximumViolations ∷ Set Violation
  maximumViolations = maybe
    Set.empty
    ( \maximum →
        if x <= maximum then Set.empty
        else Set.singleton
          { jsonPath
          , reason: InvalidRange { validRange, value: x }
          , schemaPath: Maximum : schemaPath
          }
    )
    constraints.maximum

  minimumViolations ∷ Set Violation
  minimumViolations = maybe
    Set.empty
    ( \minimum →
        if x >= minimum then Set.empty
        else Set.singleton
          { jsonPath
          , reason: InvalidRange { validRange, value: x }
          , schemaPath: Minimum : schemaPath
          }
    )
    constraints.minimum

  validRange ∷ Range
  validRange = { from: validLowerBoundary, to: validUpperBoundary }

  validLowerBoundary ∷ Boundary
  validLowerBoundary =
    case constraints.exclusiveMinimum, constraints.minimum of
      Nothing, Nothing →
        Open bottom
      Nothing, Just minimum →
        Closed minimum
      Just exclusiveMinimum, Nothing →
        Open exclusiveMinimum
      Just exclusiveMinimum, Just minimum →
        if minimum < exclusiveMinimum then Closed minimum
        else Open exclusiveMinimum

  validUpperBoundary ∷ Boundary
  validUpperBoundary =
    case constraints.exclusiveMaximum, constraints.maximum of
      Nothing, Nothing →
        Open top
      Nothing, Just maximum →
        Closed maximum
      Just exclusiveMaximum, Nothing →
        Open exclusiveMaximum
      Just exclusiveMaximum, Just maximum →
        if maximum > exclusiveMaximum then Closed maximum
        else Open exclusiveMaximum

conformsMinimum
  ∷ SchemaPath → JsonPath → Number → Maybe Number → Boolean
conformsMinimum schemaPath jsonPath x = case _ of
  Just minimum →
    x >= minimum
  Nothing →
    true

validateUniqueItems
  ∷ SchemaPath → JsonPath → Array Json → Set Violation
validateUniqueItems schemaPath jsonPath itemJsons =
  foldMapWithIndex f itemJsons
  where
  f ∷ Int → Json → Set Violation
  f itemIndex itemJson =
    if itemJson `Set.member` duplicates then
      Set.singleton
        { jsonPath: ItemIndex itemIndex : jsonPath
        , reason: NonUniqueArrayItem
        , schemaPath
        }
    else Set.empty

  duplicates ∷ Set Json
  duplicates = Map.keys $ Map.filter (_ > 1) frequencies

  frequencies ∷ Map Json Int
  frequencies = foldl
    (\acc json → Map.insertWith (+) json 1 acc)
    Map.empty
    itemJsons

validateMultipleOf
  ∷ SchemaPath → JsonPath → Number → Maybe Number → Set Violation
validateMultipleOf schemaPath jsonPath x = case _ of
  Just expectedMultiple →
    if isInteger $ x / expectedMultiple then
      Set.empty
    else Set.singleton
      { jsonPath
      , reason: InvalidMultiple { expectedMultiple, value: x }
      , schemaPath: MultipleOf : schemaPath
      }
  Nothing →
    Set.empty

validateTypeKeyword
  ∷ SchemaPath → JsonPath → Json → Set JsonValueType → Set Violation
validateTypeKeyword schemaPath jsonPath json allowedJsonValueTypes =
  if jsonValueType == JsonInteger && integersAreAllowed then Set.empty
  else if jsonValueType `Set.member` allowedJsonValueTypes then
    Set.empty
  else Set.singleton
    { jsonPath
    , reason: TypeMismatch
        { actualJsonValueType: jsonValueType, allowedJsonValueTypes }
    , schemaPath: TypeKeyword : schemaPath
    }
  where
  integersAreAllowed ∷ Boolean
  integersAreAllowed =
    JsonInteger `Set.member` allowedJsonValueTypes
      || JsonNumber `Set.member` allowedJsonValueTypes

  jsonValueType ∷ JsonValueType
  jsonValueType = A.caseJson
    (const JsonNull)
    (const JsonBoolean)
    ( \x →
        if isInteger x then JsonInteger
        else JsonNumber
    )
    (const JsonString)
    (const JsonArray)
    (const JsonObject)
    json
