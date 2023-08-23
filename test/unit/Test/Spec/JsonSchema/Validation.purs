module Test.Spec.JsonSchema.Validation (examples, spec) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Gen as AGen
import Data.Foldable (foldMap, traverse_)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Markdown (CodeBlockType(..), Document)
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Gen as StringGen
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import JsonSchema (JsonSchema(..), JsonValueType(..), Keywords)
import JsonSchema as Schema
import JsonSchema.Codec.Printing as Printing
import JsonSchema.Gen as SchemaGen
import JsonSchema.JsonPath (JsonPathSegment(..))
import JsonSchema.SchemaPath (SchemaPathSegment(..))
import JsonSchema.Validation (Violation, ViolationReason(..))
import JsonSchema.Validation as Validation
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen)
import Test.Spec (describe)
import Test.Types (Example, TestLength(..), TestSpec)
import Test.Utils (exampleTestCase, failWithDetails, generativeTestCase)

type ValidationExampleInput = { json ∷ Json, schema ∷ JsonSchema }

type ValidationExample = Example ValidationExampleInput (Set Violation)

renderInput ∷ ValidationExampleInput → Document
renderInput { json, schema } =
  [ M.heading5 "JSON schema"
  , M.codeBlock Json $ (A.stringify <<< Printing.printSchema) schema
  , M.heading5 "JSON"
  , M.codeBlock Json $ A.stringify json
  ]

renderOutput ∷ Set Violation → Document
renderOutput violations =
  [ M.codeBlock' $ String.joinWith "\n" renderViolations ]
  where
  renderViolations ∷ Array String
  renderViolations =
    if Set.isEmpty violations then [ "✓ no violations" ]
    else foldMap
      ( \violation →
          [ "✗" ]
            <> (("  " <> _) <$> Validation.renderViolation violation)
      )
      violations

transform ∷ ValidationExampleInput → Set Violation
transform { json, schema } = json `Validation.validateAgainst` schema

positiveScenario
  ∷ String → String → ValidationExampleInput → ValidationExample
positiveScenario title description input =
  { description
  , expectedOutput: Set.empty
  , input
  , renderInput
  , renderOutput
  , title
  , transform
  }

negativeScenario
  ∷ String
  → String
  → ValidationExampleInput
  → Set Violation
  → ValidationExample
negativeScenario title description input expectedViolations =
  { description
  , expectedOutput: expectedViolations
  , input
  , renderInput
  , renderOutput
  , title
  , transform
  }

examples ∷ Array ValidationExample
examples =
  [ positiveScenario
      "A null value against a schema accepting only null values"
      "A null value conforms to the schema."
      { json: A.jsonNull
      , schema: ObjectSchema $ Schema.defaultKeywords
          { typeKeyword = Just $ Set.fromFoldable [ JsonNull ] }
      }
  , negativeScenario
      "A boolean value against a schema accepting only null values"
      "A boolean value does not conform to the schema as only null values do."
      { json: A.jsonTrue
      , schema: ObjectSchema $ Schema.defaultKeywords
          { typeKeyword = Just $ Set.fromFoldable [ JsonNull ] }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: TypeMismatch
              { allowedJsonValueTypes: Set.fromFoldable [ JsonNull ]
              , actualJsonValueType: JsonBoolean
              }
          , schemaPath: TypeKeyword : Nil
          }
      )
  , positiveScenario
      "An whole number value against a schema accepting only numbers"
      "All whole number values conform to the schema as every integer is a number."
      { json: A.fromNumber 1.0
      , schema: ObjectSchema $ Schema.defaultKeywords
          { typeKeyword = Just $ Set.fromFoldable [ JsonNumber ] }
      }
  , negativeScenario
      "A fractional number value against a schema accepting only integers"
      "Not all number values conform to the schema as not every number is a integer."
      { json: A.fromNumber 1.5
      , schema: ObjectSchema $ Schema.defaultKeywords
          { typeKeyword = Just $ Set.fromFoldable [ JsonInteger ] }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: TypeMismatch
              { allowedJsonValueTypes: Set.fromFoldable [ JsonInteger ]
              , actualJsonValueType: JsonNumber
              }
          , schemaPath: TypeKeyword : Nil
          }
      )
  , negativeScenario
      "A boolean value against a schema accepting only null and string values"
      "A boolean value does not conform to the schema as only null or string values do."
      { json: A.jsonTrue
      , schema: ObjectSchema $ Schema.defaultKeywords
          { typeKeyword = Just $ Set.fromFoldable
              [ JsonNull, JsonString ]
          }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: TypeMismatch
              { allowedJsonValueTypes: Set.fromFoldable
                  [ JsonNull, JsonString ]
              , actualJsonValueType: JsonBoolean
              }
          , schemaPath: TypeKeyword : Nil
          }
      )
  , negativeScenario
      "An array with 2 out of 5 items not matching the desired item type"
      "When schema requires items to conform to a certain schema, every single value in the array has to."
      { json: A.fromArray
          [ A.jsonNull
          , A.jsonFalse
          , A.jsonNull
          , A.jsonTrue
          , A.jsonNull
          ]
      , schema: ObjectSchema $ Schema.defaultKeywords
          { items = Just $ ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.singleton JsonNull }
          , typeKeyword = Just $ Set.singleton JsonArray
          }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: InvalidArray $ Set.fromFoldable
              [ { jsonPath: ItemIndex 1 : Nil
                , reason: TypeMismatch
                    { actualJsonValueType: JsonBoolean
                    , allowedJsonValueTypes: Set.singleton JsonNull
                    }
                , schemaPath: TypeKeyword : Items : Nil
                }
              , { jsonPath: ItemIndex 3 : Nil
                , reason: TypeMismatch
                    { actualJsonValueType: JsonBoolean
                    , allowedJsonValueTypes: Set.singleton JsonNull
                    }
                , schemaPath: TypeKeyword : Items : Nil
                }
              ]
          , schemaPath: Nil
          }
      )
  , negativeScenario
      "An array with forbidden duplicate value."
      "When schema requires items to be unique, any duplicate occurrence of any value will cause a validation failure."
      { json: A.fromArray
          [ A.fromString "a"
          , A.fromString "b"
          , A.fromString "b"
          , A.fromString "c"
          , A.fromString "d"
          , A.fromString "d"
          , A.fromString "e"
          ]
      , schema: ObjectSchema
          $ Schema.defaultKeywords { uniqueItems = true }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: InvalidArray $ Set.fromFoldable
              [ { jsonPath: ItemIndex 1 : Nil
                , reason: NonUniqueArrayItem
                , schemaPath: UniqueItems : Nil
                }
              , { jsonPath: ItemIndex 2 : Nil
                , reason: NonUniqueArrayItem
                , schemaPath: UniqueItems : Nil
                }
              , { jsonPath: ItemIndex 4 : Nil
                , reason: NonUniqueArrayItem
                , schemaPath: UniqueItems : Nil
                }
              , { jsonPath: ItemIndex 5 : Nil
                , reason: NonUniqueArrayItem
                , schemaPath: UniqueItems : Nil
                }
              ]
          , schemaPath: Nil
          }
      )
  , positiveScenario
      "Number 7.5 against a schema accepting only multiples of 2.5"
      "Number 7.5 conforms to the schema as 7.5 is 2.5 times 3."
      { json: A.fromNumber 7.5
      , schema: ObjectSchema $ Schema.defaultKeywords
          { multipleOf = Just 2.5
          , typeKeyword = Just $ Set.fromFoldable [ JsonNumber ]
          }
      }
  , negativeScenario
      "Number 7 against a schema accepting only multiples of 2.5"
      "Number 7 does not conform the schema as 7.5 is not a multiple of 2.5."
      { json: A.fromNumber 7.0
      , schema:
          ObjectSchema $ Schema.defaultKeywords
            { multipleOf = Just 2.5
            , typeKeyword = Just $ Set.fromFoldable [ JsonNumber ]
            }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: InvalidMultiple
              { expectedMultiple: 2.5, value: 7.0 }
          , schemaPath: MultipleOf : Nil
          }
      )
  ]

spec ∷ TestSpec
spec = describe "Validation" do

  describe "validateAgainst" do

    traverse_ exampleTestCase examples

    keywordAppliesOnlyToProperty
      { genNonApplicableJson: AGen.genJson `Gen.suchThat`
          (not A.isArray)
      , genValidApplicableJson: A.fromArray <$>
          (Gen.unfoldable $ pure A.jsonNull)
      , jsonDescription: "array"
      , keywordName: "items"
      , keywords:
          Schema.defaultKeywords
            { items = Just $ ObjectSchema $
                Schema.defaultKeywords
                  { typeKeyword = Just $ Set.singleton JsonNull }
            }
      }

    keywordAppliesOnlyToProperty
      { genNonApplicableJson: AGen.genJson `Gen.suchThat`
          (not A.isNumber)
      , genValidApplicableJson: A.fromNumber <$> do
          i ← Gen.chooseInt (-1000) 1000
          pure $ Int.toNumber $ 2 * i
      , jsonDescription: "numeric"
      , keywordName: "multipleOf"
      , keywords: Schema.defaultKeywords { multipleOf = Just 2.0 }
      }

    keywordAppliesOnlyToProperty
      { genNonApplicableJson: AGen.genJson `Gen.suchThat`
          (not A.isObject)
      , genValidApplicableJson: do
          otherProperties ← Gen.unfoldable do
            propertyName ← StringGen.genAlphaString
            json ← AGen.genJson
            pure $ propertyName /\ json

          requiredProperty ← do
            json ← AGen.genJson
            pure $ "requiredProperty" /\ json

          pure
            $ A.fromObject
            $ Object.fromFoldable
            $ [ requiredProperty ] <> otherProperties
      , jsonDescription: "object"
      , keywordName: "required"
      , keywords:
          Schema.defaultKeywords
            { required = Set.singleton "requiredProperty"
            }
      }

    keywordAppliesOnlyToProperty
      { genNonApplicableJson: AGen.genJson `Gen.suchThat`
          (not A.isArray)
      , genValidApplicableJson: pure $ A.fromArray [ A.jsonNull ]
      , jsonDescription: "array"
      , keywordName: "uniqueItems"
      , keywords:
          Schema.defaultKeywords
            { items = Just
                $ ObjectSchema
                $ Schema.defaultKeywords { uniqueItems = true }
            }
      }

    generativeTestCase Short
      "null type accepts only null JSON values"
      do
        nonNullJsons ← Gen.unfoldable
          $ AGen.genJson `Gen.suchThat` (not A.isNull)

        nullJsons ← Gen.unfoldable
          $ pure A.jsonNull

        let
          schema = ObjectSchema $ Schema.defaultKeywords
            { typeKeyword = Just $ Set.fromFoldable [ JsonNull ] }
          violationsLists = (_ `Validation.validateAgainst` schema)
            <$> (nonNullJsons <> nullJsons)
          failedValidations = List.filter
            (not Set.isEmpty)
            violationsLists
          successfulValidations = List.filter
            Set.isEmpty
            violationsLists
          actualFailures = List.length failedValidations
          actualSuccesses = List.length successfulValidations
          expectedFailures = List.length nonNullJsons
          expectedSuccesses = List.length nullJsons

        pure
          if
            actualFailures == expectedFailures
              && actualSuccesses == expectedSuccesses then
            Success
          else failWithDetails
            "number of failed and successful validation results do not match expectations"
            { actualFailures
            , actualSuccesses
            , expectedFailures
            , expectedSuccesses
            , failedValidations
            , nonNullJsons: A.stringify <$> nonNullJsons
            , nullJsons: A.stringify <$> nullJsons
            , successfulValidations
            }

    generativeTestCase Long
      "negated schema should yield negated validation result"
      do
        originalSchema ← SchemaGen.genSchema
        json ← AGen.genJson

        let
          negatedSchema = ObjectSchema
            $ Schema.defaultKeywords { not = Just originalSchema }
          originalSchemaViolations = Set.toUnfoldable
            $ json `Validation.validateAgainst` originalSchema
          negatedSchemaViolations = Set.toUnfoldable
            $ json `Validation.validateAgainst` negatedSchema

        pure case originalSchemaViolations of
          [] →
            case negatedSchemaViolations of
              [] →
                failWithDetails
                  "validation has passed for both original and negated schemata"
                  { negatedSchema: A.stringify
                      $ Printing.printSchema negatedSchema
                  , originalSchema: A.stringify
                      $ Printing.printSchema originalSchema
                  }

              _ →
                Success
          _ →
            case negatedSchemaViolations of
              [] →
                Success
              _ →
                failWithDetails
                  "validation has failed for both original and negated schemata"
                  { negatedSchema: A.stringify
                      $ Printing.printSchema negatedSchema
                  , negatedSchemaViolations
                  , originalSchema: A.stringify
                      $ Printing.printSchema originalSchema
                  , originalSchemaViolations
                  }

keywordAppliesOnlyToProperty
  ∷ { genNonApplicableJson ∷ Gen Json
    , genValidApplicableJson ∷ Gen Json
    , jsonDescription ∷ String
    , keywordName ∷ String
    , keywords ∷ Keywords
    }
  → TestSpec
keywordAppliesOnlyToProperty spec =
  generativeTestCase Long
    ( spec.keywordName
        <> " applies only to "
        <> spec.jsonDescription
        <>
          " JSON values"
    )
    do
      json ← Gen.choose
        spec.genNonApplicableJson
        spec.genValidApplicableJson

      let
        schema = ObjectSchema spec.keywords
        violations = json `Validation.validateAgainst` schema

      pure
        if Set.isEmpty violations then
          Success
        else
          failWithDetails
            ( "validation has failed even though the only keyword was constraining only "
                <> spec.jsonDescription
            )
            { json: A.stringify json
            , schema: A.stringify $ Printing.printSchema schema
            }
