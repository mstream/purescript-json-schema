module Test.Spec.JsonSchema.Validation (examples, spec) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Gen as AGen
import Data.Foldable (foldMap, traverse_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.Codec.Printing as Printing
import JsonSchema.Gen as SchemaGen
import JsonSchema.JsonPath (JsonPathSegment(..))
import JsonSchema.SchemaPath (SchemaPathSegment(..))
import JsonSchema.Validation (Violation, ViolationReason(..))
import JsonSchema.Validation as Validation
import Test.QuickCheck (Result(..))
import Test.Spec (describe)
import Test.Types (Example, TestLength(..), TestSpec)
import Test.Utils (exampleTestCase, failWithDetails, generativeTestCase)

type ValidationExampleInput = { json ∷ Json, schema ∷ JsonSchema }

type ValidationExample = Example ValidationExampleInput (Set Violation)

renderInput ∷ ValidationExampleInput → String
renderInput { json, schema } = "##### JSON schema\n"
  <> "```json\n"
  <> (A.stringify <<< Printing.printSchema) schema
  <> "\n```\n"
  <> "##### JSON\n"
  <> "```json\n"
  <> A.stringify json
  <> "\n```"

renderOutput ∷ Set Violation → String
renderOutput violations =
  "```\n"
    <> String.joinWith "\n" renderViolations
    <> "\n```"
  where
  renderViolations ∷ Array String
  renderViolations =
    if Set.isEmpty violations then [ "✓ no violations\n" ]
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
  ]

spec ∷ TestSpec
spec = describe "Validation" do

  describe "validateAgainst" do

    traverse_ exampleTestCase examples

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
