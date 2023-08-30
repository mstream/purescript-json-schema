module Test.Spec.JsonSchema.Codec.Parsing (doc, spec) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (traverse_)
import Data.Markdown (CodeBlockType(..), Document)
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Docs.Types (Doc)
import Foreign.Object as Object
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.Codec.Parsing as Parsing
import Test.QuickCheck (Result(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Types
  ( Computation
  , Example
  , ExpectedOutput
  , Input
  , Property
  , TestSpec
  )
import Test.Utils (exampleTestCase, exampleTitle, propertyTitle)

type ParsingInput = Json
type ParsingOutput = String \/ JsonSchema

type ParsingExample = Example ParsingInput ParsingOutput
type ParsingProperty = Property ParsingInput ParsingOutput

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

computation ∷ Computation ParsingInput ParsingOutput
computation =
  { description: "parsing"
  , execute: Parsing.parseSchema
  }

renderInput ∷ Input ParsingInput → Document
renderInput { value: json } =
  [ M.paragraph "*JSON:*\n"
  , M.codeBlock Json $ A.stringifyWithIndent 2 json
  ]

renderOutput ∷ ExpectedOutput ParsingOutput → Document
renderOutput { description } =
  [ M.codeBlock' description ]

examples ∷ Array ParsingExample
examples =
  [ positiveScenario
      "A boolean value of true is a valid schema which passes validation of any JSON value"
      { description: "a 'true' boolean value"
      , value: A.jsonTrue
      }
      (BooleanSchema true)
  , positiveScenario
      "A boolean value of false is a valid schema which fails validation of any JSON value"
      { description: "a 'false' boolean value"
      , value: A.jsonFalse
      }
      (BooleanSchema false)
  , positiveScenario
      "An empty JSON object is a valid schema which passes validation of any JSON value"
      { description: "an empty JSON object"
      , value: A.jsonEmptyObject
      }
      (ObjectSchema Schema.defaultKeywords)
  , positiveScenario
      "The 'items' constrain makes sure than if a JSON value is an array, every item of that array conforms the schema defined by it."
      { description:
          "an JSON object with the 'items' property set to true"
      , value: A.fromObject $ Object.fromFoldable
          [ "items" /\ A.fromBoolean true ]
      }
      ( ObjectSchema $ Schema.defaultKeywords
          { items = Just $ BooleanSchema true
          }
      )
  , positiveScenario
      "The 'not' constrain rejects any JSON value which conform to schema defined by it."
      { description:
          "a JSON object with 'not' property set to true"
      , value: A.fromObject $ Object.fromFoldable
          [ "not" /\ A.fromBoolean true ]
      }
      ( ObjectSchema $ Schema.defaultKeywords
          { not = Just $ BooleanSchema true
          }
      )
  , positiveScenario
      "The 'required' constrain rejects any JSON object not containing properties defined by it."
      { description:
          "A JSON object with 'required' property being array of strings"
      , value: A.fromObject $ Object.fromFoldable
          [ "required" /\
              (A.fromArray $ A.fromString <$> [ "prop1", "prop2" ])
          ]
      }
      ( ObjectSchema $ Schema.defaultKeywords
          { required = Set.fromFoldable [ "prop1", "prop2" ]
          }
      )
  , positiveScenario
      "The 'type' keyword defines acceptable JSON types. It can be in a form of a single string."
      { description:
          "A JSON object with 'type' property set to 'null' string"
      , value: A.fromObject $ Object.fromFoldable
          [ "type" /\ A.fromString "null" ]
      }
      ( ObjectSchema $ Schema.defaultKeywords
          { typeKeyword = Just $ Set.singleton JsonNull }
      )
  , positiveScenario
      "The 'type' keyword defines acceptable JSON types. It can be in a form of an array of string. Here, only with one type defined."
      { description:
          "A JSON object with 'type' property set to an array with a single 'null' string inside it"
      , value: A.fromObject $ Object.fromFoldable
          [ "type" /\ A.fromArray [ A.fromString "null" ] ]
      }
      ( ObjectSchema $ Schema.defaultKeywords
          { typeKeyword = Just $ Set.singleton JsonNull }
      )
  , positiveScenario
      "The 'type' keyword defines acceptable JSON types. It can be in a form of an array of string. Here, with no types defined."
      { description:
          "A JSON object with 'type' property set to an empty array"
      , value: A.fromObject $ Object.fromFoldable
          [ "type" /\ A.fromArray [] ]
      }
      ( ObjectSchema $ Schema.defaultKeywords
          { typeKeyword = Just Set.empty }
      )
  , positiveScenario
      "The 'type' keyword defines acceptable JSON types. It can be in a form of an array of string. Here, with three types defined."
      { description:
          "A JSON object with 'type' property set to an array with 'array', 'null' and 'string' strings inside it"
      , value: A.fromObject $ Object.fromFoldable
          [ "type" /\ A.fromArray
              (A.fromString <$> [ "array", "null", "string" ])
          ]
      }
      ( ObjectSchema $ Schema.defaultKeywords
          { typeKeyword = Just $ Set.fromFoldable
              [ JsonArray, JsonNull, JsonString ]
          }

      )
  , positiveScenario
      "The 'uniqueItems' keyword makes sure that if JSON value is an array, its items do not contain any duplicates"
      { description:
          "A JSON object with 'uniqueItems' property set to true"
      , value: A.fromObject $ Object.fromFoldable
          [ "uniqueItems" /\ A.jsonTrue ]
      }
      ( ObjectSchema $ Schema.defaultKeywords
          { uniqueItems = true
          }
      )
  ]

properties ∷ Array ParsingProperty
properties = []

spec ∷ TestSpec
spec = describe "Parsing" do
  describe "parseSchema" do
    traverse_ exampleTestCase examples

positiveScenario
  ∷ String → Input ParsingInput → JsonSchema → ParsingExample
positiveScenario description input expectedSchema =
  { computation
  , description
  , expectedOutput:
      { description: "successfully parsed schema"
      , value: Right expectedSchema
      }
  , input
  , renderInput
  , renderOutput
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
