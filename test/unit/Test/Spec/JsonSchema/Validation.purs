module Test.Spec.JsonSchema.Validation (doc, spec) where

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
import Docs.Types (Doc)
import Foreign.Object as Object
import JsonSchema (JsonSchema(..), JsonValueType(..), Keywords)
import JsonSchema as Schema
import JsonSchema.Codec.Printing as Printing
import JsonSchema.Gen as SchemaGen
import JsonSchema.JsonPath (JsonPathSegment(..))
import JsonSchema.Range (Boundary(..))
import JsonSchema.SchemaPath (SchemaPathSegment(..))
import JsonSchema.Validation (Violation, ViolationReason(..))
import JsonSchema.Validation as Validation
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen)
import Test.Spec (describe)
import Test.Types
  ( Computation
  , Example
  , ExpectedOutput
  , Input
  , Property
  , TestLength(..)
  , TestSpec
  )
import Test.Utils
  ( exampleTestCase
  , exampleTitle
  , failWithDetails
  , generativeTestCase
  , propertyTitle
  )

type ValidationInput = { json ∷ Json, schema ∷ JsonSchema }
type ValidationOutput = Set Violation

type ValidationExample = Example ValidationInput ValidationOutput
type ValidationProperty = Property ValidationInput ValidationOutput

doc ∷ Doc
doc =
  { computationDescription: computation.description
  , examples: Set.fromFoldable $ examples <#> \example →
      { description: example.description
      , output: example.renderOutput example.expectedOutput
      , input: example.renderInput example.input
      , title: exampleTitle example
      }
  , properties: Set.fromFoldable $ properties <#> \property →
      { title: propertyTitle property
      }
  }

renderInput ∷ Input ValidationInput → Document
renderInput { value: { json, schema } } =
  [ M.heading5 "JSON schema"
  , M.codeBlock Json
      $ (A.stringifyWithIndent 2 <<< Printing.printSchema) schema
  , M.heading5 "JSON"
  , M.codeBlock Json $ A.stringifyWithIndent 2 json
  ]

renderOutput ∷ ExpectedOutput (Set Violation) → Document
renderOutput { value: violations } =
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

computation ∷ Computation ValidationInput ValidationOutput
computation =
  { description: "validation of"
  , execute: \{ json, schema } →
      json `Validation.validateAgainst` schema
  }

positiveScenario
  ∷ String → Input ValidationInput → ValidationExample
positiveScenario description input =
  { computation
  , description
  , expectedOutput: { description: "no violations", value: Set.empty }
  , input
  , renderInput
  , renderOutput
  }

negativeScenario
  ∷ String
  → Input ValidationInput
  → Set Violation
  → ValidationExample
negativeScenario description input expectedViolations =
  { computation
  , description
  , expectedOutput:
      { description: "violations", value: expectedViolations }
  , input
  , renderInput
  , renderOutput
  }

properties ∷ Array ValidationProperty
properties = []

examples ∷ Array ValidationExample
examples =
  [ positiveScenario
      "A null value conforms to the schema."
      { description:
          "the null value against a schema accepting only nulls"
      , value:
          { json: A.jsonNull
          , schema: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.fromFoldable [ JsonNull ] }
          }
      }
  , negativeScenario
      "A boolean value does not conform to the schema as only null values do."
      { description:
          "a boolean value against a schema accepting only nulls"
      , value:
          { json: A.jsonTrue
          , schema: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.fromFoldable [ JsonNull ] }
          }
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
      "All whole number values conform to the schema as every integer is a number."
      { description:
          "a whole number against a schema which accepts any numbers"
      , value:
          { json: A.fromNumber 1.0
          , schema: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.fromFoldable [ JsonNumber ] }
          }
      }
  , negativeScenario
      "Not all number values conform to the schema as not every number is a integer."
      { description:
          "a fractional number against a schema accepting only whole numbers"
      , value:
          { json: A.fromNumber 1.5
          , schema: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.fromFoldable [ JsonInteger ] }
          }
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
      "A boolean value does not conform to the schema as only null or string values do."
      { description:
          "a boolean value against a schema accepting only nulls and strings"
      , value:
          { json: A.jsonTrue
          , schema: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.fromFoldable
                  [ JsonNull, JsonString ]
              }
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
      "When schema requires items to conform to a certain schema, every single value in the array has to."
      { description:
          "an array containing a mixture of null and boolean values to a schema accepting only arrays of nulls"
      , value:
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
      "When schema requires items to be unique, any duplicate occurrence of any value will cause a validation failure."
      { description:
          "an array containing duplicated strings against a schema not accepting duplicates"
      , value:
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
      "Number 7.5 conforms to the schema as 7.5 is 2.5 times 3."
      { description:
          "a number which is a multiple of the factor desired by the schema"
      , value:
          { json: A.fromNumber 7.5
          , schema: ObjectSchema $ Schema.defaultKeywords
              { multipleOf = Just 2.5
              , typeKeyword = Just $ Set.fromFoldable [ JsonNumber ]
              }
          }
      }
  , negativeScenario
      "Number 7 does not conform the schema as 7.5 is not a multiple of 2.5."
      { description:
          "a number which is not a multiple of the factor desired by the schema"
      , value:
          { json: A.fromNumber 7.0
          , schema:
              ObjectSchema $ Schema.defaultKeywords
                { multipleOf = Just 2.5
                , typeKeyword = Just $ Set.fromFoldable [ JsonNumber ]
                }
          }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: InvalidMultiple
              { expectedMultiple: 2.5, value: 7.0 }
          , schemaPath: MultipleOf : Nil
          }
      )
  , positiveScenario
      "Because maximum constraint is inclusive, such a value is valid."
      { description:
          "a number which is equal to the maximum value specified by the schema"
      , value:
          { json: A.fromNumber 4.0
          , schema:
              ObjectSchema $ Schema.defaultKeywords
                { maximum = Just 4.0 }
          }
      }
  , negativeScenario
      "Because the value is out of the valid range, it is invalid."
      { description:
          "a number which is greater than the maximum value specified by the schema"
      , value:
          { json: A.fromNumber 5.0
          , schema:
              ObjectSchema $ Schema.defaultKeywords
                { maximum = Just 4.0 }
          }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: InvalidRange
              { validRange: { from: Open bottom, to: Closed 4.0 }
              , value: 5.0
              }
          , schemaPath: Maximum : Nil
          }
      )
  , positiveScenario
      "Because the value is within the valid range, it is valid."
      { description:
          "a number which is less than the exclusive maximum value specified by the schema"
      , value:
          { json: A.fromNumber 3.0
          , schema:
              ObjectSchema $ Schema.defaultKeywords
                { exclusiveMaximum = Just 4.0 }
          }
      }
  , negativeScenario
      "Because the exclusiveMaximum constraint is exclusive, such a value is invalid."
      { description:
          "a number which is equal to the exclusive maximum value specified by the schema"
      , value:
          { json: A.fromNumber 4.0
          , schema:
              ObjectSchema $ Schema.defaultKeywords
                { exclusiveMaximum = Just 4.0 }
          }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: InvalidRange
              { validRange: { from: Open bottom, to: Open 4.0 }
              , value: 4.0
              }
          , schemaPath: ExclusiveMaximum : Nil
          }
      )
  , positiveScenario
      "Because the minimum constraint is inclusive, such a value is invalid."
      { description:
          "a number which is equal to the minimum value specified by the schema"
      , value:
          { json: A.fromNumber 4.0
          , schema:
              ObjectSchema $ Schema.defaultKeywords
                { minimum = Just 4.0 }
          }
      }
  , negativeScenario
      "Because the value is out of the valid range, it is invalid."
      { description:
          "a number which is less than the minimum value specified by the schema"
      , value:
          { json: A.fromNumber 3.0
          , schema:
              ObjectSchema $ Schema.defaultKeywords
                { minimum = Just 4.0 }
          }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: InvalidRange
              { validRange: { from: Closed 4.0, to: Open top }
              , value: 3.0
              }
          , schemaPath: Minimum : Nil
          }
      )
  , positiveScenario
      "Because the value is within the valid range, it is valid."
      { description:
          "a number which is greater than the minimum value specified by the schema"
      , value:
          { json: A.fromNumber 5.0
          , schema:
              ObjectSchema $ Schema.defaultKeywords
                { exclusiveMinimum = Just 4.0 }
          }
      }
  , negativeScenario
      "Because the exclusiveMinimum constraint is exclusive, such a value is not valid."
      { description:
          "a number which is equal to the exclusive minimum value specified by the schema"
      , value:
          { json: A.fromNumber 4.0
          , schema:
              ObjectSchema $ Schema.defaultKeywords
                { exclusiveMinimum = Just 4.0 }
          }
      }
      ( Set.singleton $
          { jsonPath: Nil
          , reason: InvalidRange
              { validRange: { from: Open 4.0, to: Open top }
              , value: 4.0
              }
          , schemaPath: ExclusiveMinimum : Nil
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

        nullJsons ← Gen.unfoldable $ pure A.jsonNull

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

    generativeTestCase Short
      "both maximum and exclusiveMinimum defined with the same value"
      do
        x ← Gen.chooseFloat (-1000.0) 1000.0

        let
          schema = ObjectSchema
            $ Schema.defaultKeywords
                { exclusiveMaximum = Just x, maximum = Just x }
          json = A.fromNumber x
          schemaViolations = json `Validation.validateAgainst` schema
          expectedViolation =
            { jsonPath: Nil
            , reason: InvalidRange
                { validRange: { from: Open bottom, to: Open x }
                , value: x
                }
            , schemaPath: ExclusiveMaximum : Nil
            }

        pure case Set.toUnfoldable schemaViolations of
          [] →
            failWithDetails
              "validation has passed even though it should fail because of the exclusiveMinimum constraint"
              { json: A.stringify json
              , schema: A.stringify $ Printing.printSchema schema
              }
          [ violation ] →
            if violation == expectedViolation then Success
            else
              failWithDetails
                "violation is not as expected"
                { expectedViolation
                , json: A.stringify json
                , schema: A.stringify $ Printing.printSchema schema
                }
          violations →
            failWithDetails
              "validation has failed for more than one reason"
              { json: A.stringify json
              , schema: A.stringify $ Printing.printSchema schema
              , violations
              }

    generativeTestCase Short
      "both minimum and exclusiveMinimum defined with the same value"
      do
        x ← Gen.chooseFloat (-1000.0) 1000.0

        let
          schema = ObjectSchema
            $ Schema.defaultKeywords
                { exclusiveMinimum = Just x, minimum = Just x }
          json = A.fromNumber x
          schemaViolations = json `Validation.validateAgainst` schema
          expectedViolation =
            { jsonPath: Nil
            , reason: InvalidRange
                { validRange: { from: Open x, to: Open top }, value: x }
            , schemaPath: ExclusiveMinimum : Nil
            }

        pure case Set.toUnfoldable schemaViolations of
          [] →
            failWithDetails
              "validation has passed even though it should fail because of the exclusiveMinimum constraint"
              { json: A.stringify json
              , schema: A.stringify $ Printing.printSchema schema
              }
          [ violation ] →
            if violation == expectedViolation then Success
            else
              failWithDetails
                "violation is not as expected"
                { expectedViolation
                , json: A.stringify json
                , schema: A.stringify $ Printing.printSchema schema
                }
          violations →
            failWithDetails
              "validation has failed for more than one reason"
              { json: A.stringify json
              , schema: A.stringify $ Printing.printSchema schema
              , violations
              }

keywordAppliesOnlyToProperty
  ∷ { genNonApplicableJson ∷ Gen Json
    , genValidApplicableJson ∷ Gen Json
    , jsonDescription ∷ String
    , keywordName ∷ String
    , keywords ∷ Keywords
    }
  → TestSpec
keywordAppliesOnlyToProperty conf =
  generativeTestCase Long
    ( conf.keywordName
        <> " applies only to "
        <> conf.jsonDescription
        <>
          " JSON values"
    )
    do
      json ← Gen.choose
        conf.genNonApplicableJson
        conf.genValidApplicableJson

      let
        schema = ObjectSchema conf.keywords
        violations = json `Validation.validateAgainst` schema

      pure
        if Set.isEmpty violations then
          Success
        else
          failWithDetails
            ( "validation has failed even though the only keyword was constraining only "
                <> conf.jsonDescription
            )
            { json: A.stringify json
            , schema: A.stringify $ Printing.printSchema schema
            }
