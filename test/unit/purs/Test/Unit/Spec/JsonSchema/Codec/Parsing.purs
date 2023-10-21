module Test.Unit.Spec.JsonSchema.Codec.Parsing (spec) where

import Prelude

import Data.Argonaut.Core as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Set as Set
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.Codec.Parsing as Parsing
import JsonValue (JsonValue)
import Test.Unit.Computation
  ( ComputationContext
  , ComputationExample
  , ComputationProperty
  , ComputationSpec
  , ValueSample(..)
  , ValueSpec(..)
  )
import Type.Proxy (Proxy(..))

type Input = (json ∷ JsonValue)
type InputSpec = (json ∷ ValueSpec JsonValue)
type InputSample = (json ∷ ValueSample JsonValue)
type Output = String \/ JsonSchema
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
  , description: \{ json: ValueSpec jsonDesc } →
      StringNE.nes (Proxy ∷ Proxy "parsing ") <> jsonDesc
  , examples
  , execute: \{ json: ValueSample json } →
      Parsing.parseSchema json.sample
  , input:
      { json: ValueSpec $ StringNE.nes (Proxy ∷ Proxy "JSON value") }
  , output: ValueSpec $ StringNE.nes
      (Proxy ∷ Proxy "a JSON schema or an error")
  , properties
  }

context ∷ ComputationContext
context =
  [ M.paragraph $ M.text <$> desc ]
  where
  desc ∷ NonEmptyArray NonEmptyString
  desc =
    StringNE.nes
      ( Proxy
          ∷ Proxy "JSON schema is commonly expressed in a JSON format."
      )
      `ArrayNE.cons'`
        [ StringNE.nes
            ( Proxy
                ∷ Proxy
                    "However, not every JSON is a valid JSON schema."
            )
        ]

properties ∷ Array Property
properties = []

examples ∷ Array Example
examples =
  [ failureExample
      "booleans and objects are the only acceptable forms"
      { json: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "a JSON value not being a boolean or object"
              )
          , sample: wrap $ A.fromNumber 0.0
          }
      }
      "the JSON value is neither a boolean nor an object"
  , successExample
      "a boolean value of true is a valid schema which passes validation of any JSON value"
      { json: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "a boolean JSON value")
          , sample: wrap A.jsonTrue
          }
      }
      (BooleanSchema true)
  , successExample
      "a boolean value of false is a valid schema which passes validation of any JSON value"
      { json: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "a boolean JSON value")
          , sample: wrap A.jsonFalse
          }
      }
      (BooleanSchema false)
  , successExample
      "an empty JSON object is a valid schema which passes validation of any JSON value"
      { json: ValueSample
          { description: StringNE.nes
              ( Proxy ∷ Proxy "an empty JSON object"
              )
          , sample: wrap A.jsonEmptyObject
          }
      }
      (ObjectSchema Schema.defaultKeywords)
  , successExample
      "the 'items' constrain makes sure than if a JSON value is an array, every item of that array conforms the schema defined by it"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "an JSON object with the 'items' property defined"
                )
          , sample: wrap $ A.fromObject $ Object.fromFoldable
              [ "items" /\ A.fromBoolean true ]
          }
      }
      ( ObjectSchema Schema.defaultKeywords
          { items = Just $ BooleanSchema true
          }
      )
  , successExample
      "the 'not' constrain rejects any JSON value which conform to schema defined by it"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "an JSON object with the 'not' property defined"
                )
          , sample: wrap $ A.fromObject $ Object.fromFoldable
              [ "not" /\ A.fromBoolean true ]
          }
      }
      ( ObjectSchema Schema.defaultKeywords
          { not = Just $ BooleanSchema true
          }
      )
  , successExample
      "the 'required' constrain rejects any JSON object not containing properties defined by it"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "A JSON object with 'required' property being array of strings"
                )
          , sample: wrap $ A.fromObject $ Object.fromFoldable
              [ "required" /\
                  (A.fromArray $ A.fromString <$> [ "prop1", "prop2" ])
              ]
          }
      }
      ( ObjectSchema Schema.defaultKeywords
          { required = Set.fromFoldable [ "prop1", "prop2" ]
          }
      )
  , successExample
      "the 'type' keyword defines acceptable JSON types. It can be in a form of a single string"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "A JSON object with 'type' property defined"
                )
          , sample: wrap $ A.fromObject $ Object.fromFoldable
              [ "type" /\ A.fromString "null"
              ]
          }
      }
      ( ObjectSchema Schema.defaultKeywords
          { typeKeyword = Just $ Set.singleton JsonNull
          }
      )
  , successExample
      "the 'type' keyword defines acceptable JSON types. It can be in a form of an array of string (here, only with one type defined)"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "A JSON object with 'type' property set to an array with a single 'null' string inside it"
                )
          , sample: wrap $ A.fromObject $ Object.fromFoldable
              [ "type" /\ A.fromArray [ A.fromString "null" ] ]
          }
      }
      ( ObjectSchema Schema.defaultKeywords
          { typeKeyword = Just $ Set.singleton JsonNull }
      )
  , successExample
      "the 'type' keyword defines acceptable JSON types. It can be in a form of an array of string (here, with no types defined)"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "A JSON object with 'type' property set to an empty array"
                )
          , sample: wrap $ A.fromObject $ Object.fromFoldable
              [ "type" /\ A.fromArray [] ]
          }
      }
      ( ObjectSchema Schema.defaultKeywords
          { typeKeyword = Just Set.empty }
      )
  , successExample
      "the 'type' keyword defines acceptable JSON types. It can be in a form of an array of string (here, with three types defined)"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "A JSON object with 'type' property set to an array with 'array', 'null' and 'string' strings inside it"
                )
          , sample: wrap $ A.fromObject $ Object.fromFoldable
              [ "type" /\ A.fromArray
                  (A.fromString <$> [ "array", "null", "string" ])
              ]
          }
      }
      ( ObjectSchema Schema.defaultKeywords
          { typeKeyword = Just $ Set.fromFoldable
              [ JsonArray, JsonNull, JsonString ]
          }
      )
  , successExample
      "the 'uniqueItems' keyword makes sure that if JSON value is an array, its items do not contain any duplicates"
      { json: ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "A JSON object with 'uniqueItems' property set"
                )
          , sample: wrap $ A.fromObject $ Object.fromFoldable
              [ "uniqueItems" /\ A.jsonTrue ]
          }
      }
      ( ObjectSchema Schema.defaultKeywords
          { uniqueItems = true
          }
      )
  ]

successExample ∷ String → { | InputSample } → JsonSchema → Example
successExample description input expectedJsonSchema =
  { description:
      StringNE.nes (Proxy ∷ Proxy "Because ")
        `StringNE.appendString` description
        <> StringNE.nes
          ( Proxy
              ∷ Proxy
                  ", such a value represents a JSON schema."
          )
  , expectedOutput: ValueSample
      { description: StringNE.nes
          (Proxy ∷ Proxy "a successfully parsed JSON schema")

      , sample: Right expectedJsonSchema
      }
  , input
  }

failureExample ∷ String → { | InputSample } → String → Example
failureExample description input expectedErrorMessage =
  { description:
      StringNE.nes (Proxy ∷ Proxy "Because ")
        `StringNE.appendString` description
        <> StringNE.nes
          ( Proxy
              ∷ Proxy
                  ", such a value does not represent a JSON schema."
          )
  , expectedOutput: ValueSample
      { description: StringNE.nes
          (Proxy ∷ Proxy "a parsing error")
      , sample: Left expectedErrorMessage
      }
  , input
  }
