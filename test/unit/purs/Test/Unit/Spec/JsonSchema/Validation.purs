module Test.Unit.Spec.JsonSchema.Validation (spec) where

import Prelude

import Data.Argonaut.Core as A
import Data.Argonaut.Gen as AGen
import Data.Array.NonEmpty as ArrayNE
import Data.List (List(..), (:))
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty as SetNE
import Data.String.NonEmpty as StringNE
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.JsonPath (JsonPathSegment(..))
import JsonSchema.Range (Boundary(..))
import JsonSchema.SchemaPath (SchemaPathSegment(..))
import JsonSchema.Validation (Violation(..), ViolationReason(..))
import JsonSchema.Validation as Validation
import JsonValue (JsonValue)
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen)
import Test.Unit.Computation
  ( ComputationContext
  , ComputationExample
  , ComputationProperty
  , ComputationSpec
  , ValueSample(..)
  , ValueSpec(..)
  )
import Test.Unit.Utils (genValueSample)
import Type.Proxy (Proxy(..))

type Input = (json ∷ JsonValue, schema ∷ JsonSchema)

type InputSpec =
  (json ∷ ValueSpec JsonValue, schema ∷ ValueSpec JsonSchema)

type InputSample =
  (json ∷ ValueSample JsonValue, schema ∷ ValueSample JsonSchema)

type Output = Set Violation
type OutputSample = ValueSample Output

type Spec = ComputationSpec
  InputSample
  InputSpec
  Output
  OutputSample

type Example = ComputationExample InputSample OutputSample
type Property = ComputationProperty InputSample Output

spec ∷ Spec
spec =
  { context
  , description:
      \{ json: ValueSpec jsonDesc, schema: ValueSpec schemaDesc } →
        StringNE.nes (Proxy ∷ Proxy "validating ")
          <> jsonDesc
          <> StringNE.nes (Proxy ∷ Proxy " against ")
          <> schemaDesc
  , examples
  , execute: \{ json: ValueSample json, schema: ValueSample schema } →
      json.sample `Validation.validateAgainst` schema.sample
  , input:
      { json: ValueSpec $ StringNE.nes (Proxy ∷ Proxy "JSON value")
      , schema: ValueSpec $ StringNE.nes (Proxy ∷ Proxy "JSON schema")
      }
  , output: ValueSpec
      $ StringNE.nes (Proxy ∷ Proxy "JSON schema violations")
  , properties
  }

context ∷ ComputationContext
context =
  [ M.paragraph
      $
        ( M.text
            $
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "JSON validation is a specification for validating the structure and data types of JSON values."
                )
        )
          `ArrayNE.cons'`
            [ M.lineBreak
            , M.text
                $ StringNE.nes
                    ( Proxy
                        ∷ Proxy
                            "It allows you to specify the required properties, the types of values, the format of the data, and other constraints for a JSON object."
                    )
            , M.lineBreak
            , M.text
                $ StringNE.nes
                    ( Proxy
                        ∷ Proxy
                            "This is useful for ensuring that the data received or sent in a JSON format is as expected and can be processed correctly."
                    )
            , M.lineBreak
            , M.text
                $ StringNE.nes
                    ( Proxy
                        ∷ Proxy
                            "It helps to catch errors early, improve data quality, and reduce the amount of code needed for data validation."
                    )
            ]
  ]

properties ∷ Array Property
properties =
  [ { description:
        StringNE.nes
          ( Proxy
              ∷ Proxy
                  "'true' JSON schema does not impose any constraints"
          )
    , property: \execute → do
        json ← genAnyJsonValueSample

        let
          schema = ValueSample
            { description: StringNE.nes
                (Proxy ∷ Proxy "'true' JSON schema")
            , sample: BooleanSchema true
            }
          violations = execute { json, schema }

        pure
          if Set.isEmpty violations then Success
          else Failed
            $ "the validation reported violations: "
                <> show violations
    }
  , { description:
        StringNE.nes
          (Proxy ∷ Proxy "'false' JSON schema rejects anything")
    , property: \execute → do
        json ← genAnyJsonValueSample

        let
          schema = ValueSample
            { description: StringNE.nes
                (Proxy ∷ Proxy "'false' JSON schema")
            , sample: BooleanSchema false
            }
          violations = execute { json, schema }

        pure
          if Set.isEmpty violations then Failed
            "the validation did not report any violations"
          else Success
    }
  , { description: StringNE.nes
        ( Proxy
            ∷ Proxy
                "any JSON value passes validation against 'empty object' JSON schema"
        )
    , property: \execute → do
        json ← genAnyJsonValueSample

        let
          schema = ValueSample
            { description: StringNE.nes
                (Proxy ∷ Proxy "'empty object' JSON schema")
            , sample: ObjectSchema $ Schema.defaultKeywords
            }
          violations = execute { json, schema }

        pure
          if Set.isEmpty violations then Success
          else Failed
            $ "the validation reported violations: "
                <> show violations
    }
  ]

genAnyJsonValueSample ∷ Gen (ValueSample JsonValue)
genAnyJsonValueSample = genValueSample
  (StringNE.nes (Proxy ∷ Proxy "any JSON value"))
  (wrap <$> AGen.genJson)

examples ∷ Array Example
examples =
  [ noViolationsExample
      "a JSON value directly matches schema's only 'type' keyword item"
      { json: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "JSON number value")
          , sample: wrap $ A.fromNumber 2.5
          }
      , schema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "JSON schema accepting only numbers")
          , sample: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.fromFoldable [ JsonNumber ] }
          }
      }
  , noViolationsExample
      "a JSON value indirectly matches schema's only 'type' keyword item"
      { json: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON number value which happens to be an integer"
              )
          , sample: wrap $ A.fromNumber 1.0
          }
      , schema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "JSON schema accepting any numbers")
          , sample: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.fromFoldable [ JsonNumber ] }
          }
      }
  , noViolationsExample
      "a JSON value directly matches one of schema's 'type' keyword items"
      { json: ValueSample
          { description: StringNE.nes (Proxy ∷ Proxy "JSON null value")
          , sample: wrap A.jsonNull
          }
      , schema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON schema accepting booleans, nulls and strings"
              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.fromFoldable
                  [ JsonBoolean, JsonNull, JsonString ]
              }
          }
      }
  , noViolationsExample
      "a JSON number value is a multiple of the factor desired by the schema"
      { json: ValueSample
          { description: StringNE.nes (Proxy ∷ Proxy "a multiple of x")
          , sample: wrap $ A.fromNumber 7.5
          }
      , schema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "schema accepting only numbers which are multiples of x"
              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { multipleOf = Just 2.5
              , typeKeyword = Just $ Set.fromFoldable [ JsonNumber ]
              }
          }
      }
  , violationsExample
      "the value is neither null or string"
      { json: ValueSample
          { description: StringNE.nes (Proxy ∷ Proxy "a boolean value")
          , sample: wrap $ A.jsonTrue
          }
      , schema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "schema accepting only nulls or strings")
          , sample: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just
                  $ Set.fromFoldable [ JsonNull, JsonString ]
              }
          }
      }
      ( ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "a type mismatch violation")
          , sample: Set.singleton $ Violation
              { jsonPath: Nil
              , reason: TypeMismatch
                  { actualJsonValueType: JsonBoolean
                  , allowedJsonValueTypes: Set.fromFoldable
                      [ JsonNull, JsonString ]
                  }
              , schemaPath: TypeKeyword : Nil
              }
          }
      )
  , violationsExample
      "the schema accepts only whole numbers"
      { json: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "a fractional number")
          , sample: wrap $ A.fromNumber 1.5
          }
      , schema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "schema accepting only whole numbers")
          , sample: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.singleton JsonInteger }
          }
      }
      ( ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "a type mismatch violation")
          , sample: Set.singleton $ Violation
              { jsonPath: Nil
              , reason: TypeMismatch
                  { actualJsonValueType: JsonNumber
                  , allowedJsonValueTypes: Set.singleton JsonInteger
                  }
              , schemaPath: TypeKeyword : Nil
              }
          }
      )
  , violationsExample
      "the schema requires items to conform to a certain schema and this is not the case here"
      { json: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "an array containing a mixture of null and boolean values to a schema accepting only arrays of nulls"
              )
          , sample: wrap $ A.fromArray
              [ A.jsonNull
              , A.jsonFalse
              , A.jsonNull
              , A.jsonTrue
              , A.jsonNull
              ]

          }
      , schema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "schema accepting only arrays of nulls")
          , sample: ObjectSchema $ Schema.defaultKeywords
              { items = Just $ ObjectSchema $ Schema.defaultKeywords
                  { typeKeyword = Just $ Set.singleton JsonNull }
              , typeKeyword = Just $ Set.singleton JsonArray
              }
          }
      }
      ( ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "an invalid array violation")
          , sample: Set.singleton $ Violation
              { jsonPath: Nil
              , reason: InvalidArray $
                  Violation
                    { jsonPath: ItemIndex 1 : Nil
                    , reason: TypeMismatch
                        { actualJsonValueType: JsonBoolean
                        , allowedJsonValueTypes: Set.singleton JsonNull
                        }
                    , schemaPath: TypeKeyword : Items : Nil
                    } `SetNE.cons` Set.fromFoldable
                    [ Violation
                        { jsonPath: ItemIndex 3 : Nil
                        , reason: TypeMismatch
                            { actualJsonValueType: JsonBoolean
                            , allowedJsonValueTypes: Set.singleton
                                JsonNull
                            }
                        , schemaPath: TypeKeyword : Items : Nil
                        }
                    ]
              , schemaPath: Nil
              }
          }
      )
  , violationsExample
      "the schema requires items to be unique, and the value contains duplicate occurrence"
      { json: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy "an array containing some duplicated strings"
              )
          , sample: wrap $ A.fromArray $ A.fromString <$>
              [ "a", "b", "b", "c", "d", "d", "e" ]
          }
      , schema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "schema not accepting duplicates")
          , sample: ObjectSchema $ Schema.defaultKeywords
              { uniqueItems = true }
          }
      }
      ( ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "an invalid array violation")
          , sample: Set.singleton $ Violation
              { jsonPath: Nil
              , reason: InvalidArray $
                  Violation
                    { jsonPath: ItemIndex 1 : Nil
                    , reason: NonUniqueArrayItem
                    , schemaPath: UniqueItems : Nil
                    } `SetNE.cons`
                    Set.fromFoldable
                      [ Violation
                          { jsonPath: ItemIndex 2 : Nil
                          , reason: NonUniqueArrayItem
                          , schemaPath: UniqueItems : Nil
                          }
                      , Violation
                          { jsonPath: ItemIndex 4 : Nil
                          , reason: NonUniqueArrayItem
                          , schemaPath: UniqueItems : Nil
                          }
                      , Violation
                          { jsonPath: ItemIndex 5 : Nil
                          , reason: NonUniqueArrayItem
                          , schemaPath: UniqueItems : Nil
                          }
                      ]
              , schemaPath: Nil
              }
          }
      )
  , noViolationsExample
      "the schema accepts any multiples of 2.5"
      { json: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "a multiple of 2.5")
          , sample: wrap $ A.fromNumber 7.5
          }
      , schema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "a schema accepting only multiples of 2.5")
          , sample: ObjectSchema $ Schema.defaultKeywords
              { multipleOf = Just 2.5
              , typeKeyword = Just $ Set.fromFoldable [ JsonNumber ]
              }
          }
      }
  , violationsExample
      "the schema accepts only multiples of 2.5"
      { json: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "not a multiple of 2.5")
          , sample: wrap $ A.fromNumber 7.0
          }
      , schema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "a schema accepting only multiples of 2.5")
          , sample: ObjectSchema $ Schema.defaultKeywords
              { multipleOf = Just 2.5
              , typeKeyword = Just $ Set.fromFoldable [ JsonNumber ]
              }
          }
      }
      ( ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "an invalid multiple violation")
          , sample: Set.singleton $ Violation
              { jsonPath: Nil
              , reason: InvalidMultiple
                  { expectedMultiple: 2.5, value: 7.0 }
              , schemaPath: MultipleOf : Nil
              }
          }
      )
  , noViolationsExample
      "the maximum value constraint is inclusive"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "number at the schema's maximum allowed values boundary"
                )
          , sample: wrap $ A.fromNumber 4.0
          }
      , schema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "a schema with a maximum inclusive allowed value set"
              )
          , sample:
              ObjectSchema $ Schema.defaultKeywords
                { maximum = Just 4.0 }
          }
      }
  , violationsExample
      "the maximum value constraint is exclusive"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "number at the schema's maximum allowed values boundary"
                )
          , sample: wrap $ A.fromNumber 4.0
          }
      , schema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "a schema with a maximum exclusive allowed value set"
              )
          , sample:
              ObjectSchema $ Schema.defaultKeywords
                { exclusiveMaximum = Just 4.0 }
          }
      }
      ( ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "an invalid range violation")
          , sample: Set.singleton $ Violation
              { jsonPath: Nil
              , reason: InvalidRange
                  { validRange: { from: Open bottom, to: Open 4.0 }
                  , value: 4.0
                  }
              , schemaPath: ExclusiveMaximum : Nil
              }
          }
      )
  , violationsExample
      "the value is greater than the maximum value constraint is exclusive"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "number exceeding the schema's maximum allowed values boundary"
                )
          , sample: wrap $ A.fromNumber 5.0
          }
      , schema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "a schema with a maximum inclusive allowed value set"
              )
          , sample:
              ObjectSchema $ Schema.defaultKeywords
                { maximum = Just 4.0 }
          }
      }
      ( ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "an invalid range violation")
          , sample: Set.singleton $ Violation
              { jsonPath: Nil
              , reason: InvalidRange
                  { validRange: { from: Open bottom, to: Closed 4.0 }
                  , value: 5.0
                  }
              , schemaPath: Maximum : Nil
              }
          }
      )
  , noViolationsExample
      "the value is less than the maximum value constraint"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "number below the schema's maximum allowed values boundary"
                )
          , sample: wrap $ A.fromNumber 3.0
          }
      , schema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "a schema with a maximum exclusive allowed value set"
              )
          , sample:
              ObjectSchema $ Schema.defaultKeywords
                { exclusiveMaximum = Just 4.0 }
          }
      }
  , noViolationsExample
      "the minimum value constraint is inclusive"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "number at the schema's minimum allowed values boundary"
                )
          , sample: wrap $ A.fromNumber 4.0
          }
      , schema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "a schema with a minimum inclusive allowed value set"
              )
          , sample:
              ObjectSchema $ Schema.defaultKeywords
                { minimum = Just 4.0 }
          }
      }
  , violationsExample
      "the minimum value constraint is exclusive"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "number at the schema's minimum allowed values boundary"
                )
          , sample: wrap $ A.fromNumber 4.0
          }
      , schema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "a schema with a minimum exclusive allowed value set"
              )
          , sample:
              ObjectSchema $ Schema.defaultKeywords
                { exclusiveMinimum = Just 4.0 }
          }
      }
      ( ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "an invalid range violation")
          , sample: Set.singleton $ Violation
              { jsonPath: Nil
              , reason: InvalidRange
                  { validRange: { from: Open 4.0, to: Open top }
                  , value: 4.0
                  }
              , schemaPath: ExclusiveMinimum : Nil
              }
          }
      )
  , violationsExample
      "the value is greater than the minimum value constraint is exclusive"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "number exceeding the schema's minimum allowed values boundary"
                )
          , sample: wrap $ A.fromNumber 3.0
          }
      , schema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "a schema with a minimum inclusive allowed value set"
              )
          , sample:
              ObjectSchema $ Schema.defaultKeywords
                { minimum = Just 4.0 }
          }
      }
      ( ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "an invalid range violation")
          , sample: Set.singleton $ Violation
              { jsonPath: Nil
              , reason: InvalidRange
                  { validRange: { from: Closed 4.0, to: Open top }
                  , value: 3.0
                  }
              , schemaPath: Minimum : Nil
              }
          }
      )
  , noViolationsExample
      "the value is less than the minimum value constraint"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "number below the schema's minimum allowed values boundary"
                )
          , sample: wrap $ A.fromNumber 5.0
          }
      , schema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "a schema with a minimum exclusive allowed value set"
              )
          , sample:
              ObjectSchema $ Schema.defaultKeywords
                { exclusiveMinimum = Just 4.0 }
          }
      }
  ]

noViolationsExample ∷ String → { | InputSample } → Example
noViolationsExample justification input =
  { description:
      StringNE.nes (Proxy ∷ Proxy "Because ")
        `StringNE.appendString` justification
        <>
          StringNE.nes (Proxy ∷ Proxy ", such a value is valid.")
  , expectedOutput: ValueSample
      { description: StringNE.nes (Proxy ∷ Proxy "no violations")
      , sample: Set.empty
      }
  , input
  }

violationsExample
  ∷ String → { | InputSample } → ValueSample Output → Example
violationsExample justification input expectedOutput =
  { description:
      StringNE.nes (Proxy ∷ Proxy "Because ")
        `StringNE.appendString` justification
        <>
          StringNE.nes (Proxy ∷ Proxy ", such a value is invalid.")
  , expectedOutput
  , input
  }
