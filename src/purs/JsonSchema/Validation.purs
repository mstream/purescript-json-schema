module JsonSchema.Validation
  ( Violation(..)
  , ViolationReason(..)
  , validateAgainst
  ) where

import Prelude

import Data.Argonaut.Core as A
import Data.Array as Array
import Data.Array.NonEmpty as ArrayNE
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Markdown as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.NonEmpty as NE
import Data.Semigroup.Foldable (foldMap1)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as SetNE
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.NonEmpty as StringNE
import Docs.Document (class Document, document)
import JsonSchema (JsonSchema(..), JsonValueType(..), Keywords)
import JsonSchema as Schema
import JsonSchema.JsonPath (JsonPath, JsonPathSegment(..))
import JsonSchema.JsonPath as JsonPath
import JsonSchema.Range (Boundary(..), Range)
import JsonSchema.Range as Range
import JsonSchema.SchemaPath (SchemaPath, SchemaPathSegment(..))
import JsonSchema.SchemaPath as SchemaPath
import JsonValue (JsonValue)
import Type.Proxy (Proxy(..))
import Utils (isInteger)

newtype Violation = Violation
  { jsonPath ∷ JsonPath
  , reason ∷ ViolationReason
  , schemaPath ∷ SchemaPath
  }

derive instance Eq Violation
derive instance Generic Violation _
derive instance Ord Violation

instance Show Violation where
  show = genericShow

instance Document Violation where
  document (Violation { jsonPath, reason, schemaPath }) =
    ( M.paragraph $
        (M.text $ StringNE.nes (Proxy ∷ Proxy "JSON value path: "))
          `ArrayNE.cons'`
            [ M.inlineCode $ JsonPath.render jsonPath
            , M.lineBreak
            , M.text $ StringNE.nes (Proxy ∷ Proxy "JSON schema path: ")
            , M.inlineCode $ SchemaPath.render schemaPath
            ]
    ) :| (Array.fromFoldable $ document reason)

data ViolationReason
  = AlwaysFailingSchema
  | InvalidArray (NonEmptySet Violation)
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

instance Document ViolationReason where
  document = case _ of
    AlwaysFailingSchema →
      NE.singleton $ M.paragraph $ ArrayNE.singleton $ M.text $
        StringNE.nes
          (Proxy ∷ Proxy "Schema always fails validation.")
    InvalidArray itemViolations →
      ( M.paragraph $ ArrayNE.singleton $ M.text $ StringNE.nes
          (Proxy ∷ Proxy "Invalid array:")
      )
        :|
          [ M.unorderedList
              $ foldMap1
                  ( ArrayNE.singleton
                      <<< ArrayNE.fromNonEmpty
                      <<< document
                  )
                  itemViolations
          ]
    InvalidMultiple { expectedMultiple, value } →
      NE.singleton $ M.paragraph $ ArrayNE.singleton $ M.text $
        StringNE.nes
          ( Proxy
              ∷ Proxy
                  " is not a multiple of "
          ) `StringNE.appendString` show expectedMultiple
    InvalidRange { validRange, value } →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $
          show value
            `StringNE.prependString` StringNE.nes
              (Proxy ∷ Proxy " is outside of the valid range of ")
            <> Range.renderRange validRange
    NonUniqueArrayItem →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes (Proxy ∷ Proxy "Non-unique array item.")
    TypeMismatch { actualJsonValueType, allowedJsonValueTypes } →
      NE.singleton $ M.paragraph $ ArrayNE.singleton $ M.text $
        StringNE.nes (Proxy ∷ Proxy "Invalid type. Expected ")
          <>
            ( case ArrayNE.fromFoldable allowedJsonValueTypes of
                Nothing →
                  StringNE.nes (Proxy ∷ Proxy "none")
                Just allowedTypes →
                  StringNE.join1With "or"
                    $ Schema.renderJsonValueType <$> allowedTypes
            )
          <> StringNE.nes (Proxy ∷ Proxy " but got ")
          <> Schema.renderJsonValueType actualJsonValueType
          <> StringNE.nes (Proxy ∷ Proxy ".")
    ValidAgainstNotSchema →
      NE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes
            (Proxy ∷ Proxy "JSON is valid against schema from 'not'.")

validateAgainst ∷ JsonValue → JsonSchema → Set Violation
validateAgainst = go Nil Nil
  where
  go ∷ SchemaPath → JsonPath → JsonValue → JsonSchema → Set Violation
  go schemaPath jsonPath json schema = case schema of
    BooleanSchema bool →
      if bool then Set.empty
      else Set.singleton $ Violation
        { jsonPath, reason: AlwaysFailingSchema, schemaPath }

    ObjectSchema keywords →
      validateAgainstObjectSchema schemaPath jsonPath json keywords

  validateAgainstObjectSchema
    ∷ SchemaPath → JsonPath → JsonValue → Keywords → Set Violation
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
                itemViolations = validateArray
                  schemaPath
                  jsonPath
                  (wrap <$> array)
                  keywords
              in
                case SetNE.fromSet itemViolations of
                  Just violations →
                    Set.singleton $ Violation
                      { jsonPath
                      , reason: InvalidArray violations
                      , schemaPath
                      }
                  Nothing →
                    Set.empty
            else Set.empty
      )
      (const Set.empty)
      (unwrap json)
    where
    notViolations ∷ Set Violation
    notViolations = case keywords.not of
      Just schema →
        if Set.isEmpty $ validateAgainst json schema then
          Set.singleton $ Violation
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
    → Array JsonValue
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
    ∷ SchemaPath
    → JsonPath
    → Array JsonValue
    → JsonSchema
    → Set Violation
  validateItems schemaPath jsonPath itemJsons schema =
    foldMapWithIndex f itemJsons
    where
    f ∷ Int → JsonValue → Set Violation
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
        else Set.singleton $ Violation
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
        else Set.singleton $ Violation
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
        else Set.singleton $ Violation
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
        else Set.singleton $ Violation
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

validateUniqueItems
  ∷ SchemaPath → JsonPath → Array JsonValue → Set Violation
validateUniqueItems schemaPath jsonPath itemJsons =
  foldMapWithIndex f itemJsons
  where
  f ∷ Int → JsonValue → Set Violation
  f itemIndex itemJson =
    if itemJson `Set.member` duplicates then
      Set.singleton $ Violation
        { jsonPath: ItemIndex itemIndex : jsonPath
        , reason: NonUniqueArrayItem
        , schemaPath
        }
    else Set.empty

  duplicates ∷ Set JsonValue
  duplicates = Map.keys $ Map.filter (_ > 1) frequencies

  frequencies ∷ Map JsonValue Int
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
    else Set.singleton $ Violation
      { jsonPath
      , reason: InvalidMultiple { expectedMultiple, value: x }
      , schemaPath: MultipleOf : schemaPath
      }
  Nothing →
    Set.empty

validateTypeKeyword
  ∷ SchemaPath
  → JsonPath
  → JsonValue
  → Set JsonValueType
  → Set Violation
validateTypeKeyword schemaPath jsonPath json allowedJsonValueTypes =
  if jsonValueType == JsonInteger && integersAreAllowed then Set.empty
  else if jsonValueType `Set.member` allowedJsonValueTypes then
    Set.empty
  else Set.singleton $ Violation
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
    (unwrap json)
