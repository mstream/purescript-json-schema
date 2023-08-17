module Test.Spec.JsonSchema.Codec.Parsing (examples, spec) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.Codec.Parsing as Parsing
import Test.QuickCheck (Result(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Types (Example, TestSpec)

type ParsingExample = Example Unit Unit

examples ∷ Array ParsingExample
examples = []

spec ∷ TestSpec
spec = describe "Parsing" do
  positiveTestCase
    { description: "an empty object schema"
    , expectedSchema: ObjectSchema Schema.defaultKeywords
    , json: A.jsonEmptyObject
    }

  positiveTestCase
    { description: "object schema with items schema"
    , expectedSchema: ObjectSchema $ Schema.defaultKeywords
        { items = Just $ BooleanSchema true
        }
    , json: A.fromObject $ Object.fromFoldable
        [ "items" /\ A.fromBoolean true ]
    }

  positiveTestCase
    { description: "object schema with negated schema"
    , expectedSchema: ObjectSchema $ Schema.defaultKeywords
        { not = Just $ BooleanSchema true
        }
    , json: A.fromObject $ Object.fromFoldable
        [ "not" /\ A.fromBoolean true ]
    }

  positiveTestCase
    { description: "object schema with required property names"
    , expectedSchema: ObjectSchema $ Schema.defaultKeywords
        { required = Set.fromFoldable [ "prop1", "prop2" ]
        }
    , json: A.fromObject $ Object.fromFoldable
        [ "required" /\
            (A.fromArray $ A.fromString <$> [ "prop1", "prop2" ])
        ]
    }

  positiveTestCase
    { description: "object schema with undefined JSON value types"
    , expectedSchema: ObjectSchema $ Schema.defaultKeywords
        { typeKeyword = Nothing }
    , json: A.jsonEmptyObject
    }

  positiveTestCase
    { description: "object schema with empty JSON value types"
    , expectedSchema: ObjectSchema $ Schema.defaultKeywords
        { typeKeyword = Just Set.empty }
    , json: A.fromObject $ Object.fromFoldable
        [ "type" /\ A.fromArray [] ]
    }

  positiveTestCase
    { description: "object schema with multiple JSON value types"
    , expectedSchema: ObjectSchema $ Schema.defaultKeywords
        { typeKeyword = Just $ Set.fromFoldable
            [ JsonArray, JsonNull, JsonString ]
        }
    , json: A.fromObject $ Object.fromFoldable
        [ "type" /\ A.fromArray
            (A.fromString <$> [ "array", "null", "string" ])
        ]
    }

  positiveTestCase
    { description: "object schema with expected unique items"
    , expectedSchema: ObjectSchema $ Schema.defaultKeywords
        { uniqueItems = true
        }
    , json: A.fromObject $ Object.fromFoldable
        [ "uniqueItems" /\ A.jsonTrue ]
    }

positiveTestCase
  ∷ { description ∷ String, expectedSchema ∷ JsonSchema, json ∷ Json }
  → TestSpec
positiveTestCase { description, expectedSchema, json } = it
  ("Should parse " <> description)
  case Parsing.parseSchema json of
    Left parseErrorMessage →
      fail $ "Could not parse JSON schema: " <> parseErrorMessage
    Right parsedSchema →
      parsedSchema `shouldEqual` expectedSchema

errorMessageShouldMention
  ∷ { description ∷ String
    , parsingResult ∷ String \/ JsonSchema
    , pattern ∷ String
    }
  → Result
errorMessageShouldMention { description, parsingResult, pattern } =
  case parsingResult of
    Left errorMessage →
      if String.contains (Pattern pattern) errorMessage then
        Success
      else Failed $ "Error message does not mention "
        <> description
        <> ": "
        <>
          errorMessage
    Right parsedSchema →
      Failed $ "Schema was not supposed to be parsed: " <> show
        { parsedSchema }
