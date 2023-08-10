module Test.Spec.JsonSchema.Validation (spec) where

import Prelude

import Control.Alternative ((<|>))
import Control.Alternative as Map
import Control.Lazy (class Lazy)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Gen as AGen
import Data.Int as Int
import Data.List (List)
import Data.Maybe (Maybe(..), isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Gen as StringGen
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Foreign.Object as Object
import JsonSchema (JsonSchema(..))
import JsonSchema as JsonSchema
import JsonSchema.Validation as Validation
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen)
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "Validation" do

  describe "validateAgainst" do

    describe "empty-object schema" do

      positiveTestCase
        { jsonSpec:
            { description: "All JSONs"
            , gen: AGen.genJson
            }
        , schemaSpec:
            { description: "empty-object schemata"
            , gen: pure JsonEmptySchema
            }
        }

    describe "array schema" do

      positiveTestCase
        { jsonSpec:
            { description: "All array JSONs"
            , gen: A.fromArray <$> Gen.unfoldable AGen.genJson
            }
        , schemaSpec:
            { description: "unrestricted array schemata"
            , gen: pure $ JsonArraySchema
                { itemsSchema: Nothing, uniqueItems: false }
            }
        }

      negativeTestCase
        { jsonSpec:
            { description: "All non-array JSONs"
            , gen: AGen.genJson `Gen.suchThat` not A.isArray
            }
        , schemaSpec:
            { description: "any array schema"
            , gen: JsonSchema.genArraySchema
            }
        }

    describe "boolean schema" do

      positiveTestCase
        { jsonSpec:
            { description: "All boolean JSONs"
            , gen: A.fromBoolean <$> (pure false <|> pure true)
            }
        , schemaSpec:
            { description: "any boolean schema"
            , gen: pure JsonBooleanSchema
            }
        }

      negativeTestCase
        { jsonSpec:
            { description: "All non-boolean JSONs"
            , gen: AGen.genJson `Gen.suchThat` not A.isBoolean
            }
        , schemaSpec:
            { description: "any boolean schema"
            , gen: pure JsonBooleanSchema
            }
        }

    describe "integer schema" do

      positiveTestCase
        { jsonSpec:
            { description: "All integer JSONs"
            , gen: A.fromNumber
                <$> Int.toNumber
                <$> Gen.chooseInt bottom top
            }
        , schemaSpec:
            { description: "unrestricted integer schemata"
            , gen: pure $ JsonIntegerSchema {}
            }
        }

      negativeTestCase
        { jsonSpec:
            { description: "All non-integer JSONs"
            , gen: AGen.genJson `Gen.suchThat`
                A.caseJsonNumber true (isNothing <<< Int.fromNumber)
            }
        , schemaSpec:
            { description: "any integer schema"
            , gen: JsonSchema.genIntegerSchema
            }
        }

    describe "null schema" do

      positiveTestCase
        { jsonSpec:
            { description: "Null JSONs"
            , gen: pure A.jsonNull
            }
        , schemaSpec:
            { description: "null schemata"
            , gen: pure JsonNullSchema
            }
        }

      negativeTestCase
        { jsonSpec:
            { description: "All non-null JSONs"
            , gen: AGen.genJson `Gen.suchThat` not A.isNull
            }
        , schemaSpec:
            { description: "null schemata"
            , gen: pure JsonNullSchema
            }
        }

    describe "number schema" do
      positiveTestCase
        { jsonSpec:
            { description: "All number JSONs"
            , gen: A.fromNumber <$> Gen.chooseFloat bottom top
            }
        , schemaSpec:
            { description: "unrestricted number schemata"
            , gen: pure $ JsonNumberSchema {}
            }
        }

      negativeTestCase
        { jsonSpec:
            { description: "All non-number JSONs"
            , gen: AGen.genJson `Gen.suchThat` not A.isNumber
            }
        , schemaSpec:
            { description: "any number schema"
            , gen: JsonSchema.genNumberSchema
            }
        }

    describe "object schema" do
      positiveTestCase
        { jsonSpec:
            { description: "All object JSONs"
            , gen: A.fromObject
                <$> Object.fromFoldable
                <$> genKeyValuePairs
            }
        , schemaSpec:
            { description: "unrestricted object schemata"
            , gen: pure $ JsonObjectSchema { properties: Map.empty }
            }
        }

      negativeTestCase
        { jsonSpec:
            { description: "All non-object JSONs"
            , gen: AGen.genJson `Gen.suchThat` not A.isObject
            }
        , schemaSpec:
            { description: "any object schema"
            , gen: JsonSchema.genObjectSchema
            }
        }

    describe "string schema" do
      positiveTestCase
        { jsonSpec:
            { description: "All string JSONs"
            , gen: A.fromString <$> StringGen.genUnicodeString
            }
        , schemaSpec:
            { description: "unrestricted string schemata"
            , gen: pure $ JsonStringSchema {}
            }
        }

      negativeTestCase
        { jsonSpec:
            { description: "All non-string JSONs"
            , gen: AGen.genJson `Gen.suchThat` not A.isString
            }
        , schemaSpec:
            { description: "any string schema"
            , gen: JsonSchema.genStringSchema
            }
        }

genKeyValuePairs
  ∷ ∀ m
  . Lazy (m Json)
  ⇒ MonadGen m
  ⇒ MonadRec m
  ⇒ m (List (String /\ Json))
genKeyValuePairs =
  Gen.unfoldable genKeyValuePair
  where
  genKeyValuePair = Tuple
    <$> StringGen.genUnicodeString
    <*> AGen.genJson

type GenSpec a =
  { description ∷ String
  , gen ∷ Gen a
  }

positiveTestCase
  ∷ { jsonSpec ∷ GenSpec Json, schemaSpec ∷ GenSpec JsonSchema }
  → TestSpec
positiveTestCase { jsonSpec, schemaSpec } =
  generativeTestCase
    Long
    ( jsonSpec.description
        <> " do not violate "
        <> schemaSpec.description
    )
    do
      json ← jsonSpec.gen
      schema ← schemaSpec.gen
      pure $ json `shouldNotViolate` schema

negativeTestCase
  ∷ { jsonSpec ∷ GenSpec Json, schemaSpec ∷ GenSpec JsonSchema }
  → TestSpec
negativeTestCase { jsonSpec, schemaSpec } =
  generativeTestCase
    Long
    ( jsonSpec.description
        <> " do violate "
        <> schemaSpec.description
    )
    do
      json ← jsonSpec.gen
      schema ← schemaSpec.gen
      pure $ json `shouldViolate` schema

shouldNotViolate ∷ Json → JsonSchema → Result
shouldNotViolate json schema =
  if Set.isEmpty violations then Success
  else Failed
    $ "Unexpected violations found: "
        <> show { json: A.stringify json, schema, violations }
  where
  violations ∷ Set String
  violations = json `Validation.validateAgainst` schema

shouldViolate ∷ Json → JsonSchema → Result
shouldViolate json schema =
  if Set.isEmpty violations then Failed
    $ "No violations found: " <> show { json: A.stringify json, schema }
  else Success
  where
  violations ∷ Set String
  violations = json `Validation.validateAgainst` schema
