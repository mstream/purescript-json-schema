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
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object as Object
import JsonSchema (JsonSchema(..))
import JsonSchema as JsonSchema
import JsonSchema.Validation as Validation
import Test.QuickCheck (Result(..))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "Validation" do

  describe "violations" do

    generativeTestCase Long
      "No JSON violates empty schema."
      do
        json ← AGen.genJson
        pure $ json `shouldNotViolate` JsonEmptySchema

    generativeTestCase Long
      "No array JSON violates unrestricted array schema."
      do
        json ← A.fromArray <$> Gen.unfoldable AGen.genJson
        pure $ json `shouldNotViolate` JsonArraySchema
          { itemsSchema: Nothing, uniqueItems: false }

    generativeTestCase Long
      "Any non-array JSON violates any array schema."
      do
        json ← AGen.genJson `Gen.suchThat` not A.isArray
        schema ← JsonSchema.genArraySchema
        pure $ json `shouldViolate` schema

    generativeTestCase Long
      "No boolean JSON violates boolean schema."
      do
        json ← A.fromBoolean <$> (pure false <|> pure true)
        pure $ json `shouldNotViolate` JsonBooleanSchema

    generativeTestCase Long
      "Any non-boolean JSON violates boolean schema."
      do
        json ← AGen.genJson `Gen.suchThat` not A.isBoolean
        pure $ json `shouldViolate` JsonBooleanSchema

    generativeTestCase Long
      "No integer JSON violates unrestricted integer schema."
      do
        json ← A.fromNumber
          <$> Int.toNumber
          <$> Gen.chooseInt bottom top
        pure $ json `shouldNotViolate` (JsonIntegerSchema {})

    generativeTestCase Long
      "Any non-integer JSON violates any integer schema."
      do
        json ← AGen.genJson `Gen.suchThat`
          A.caseJsonNumber true (isNothing <<< Int.fromNumber)
        schema ← JsonSchema.genIntegerSchema
        pure $ json `shouldViolate` schema

    generativeTestCase Long
      "No null JSON violates null schema."
      do
        json ← pure A.jsonNull
        pure $ json `shouldNotViolate` JsonNullSchema

    generativeTestCase Long
      "Any non-null JSON violates null schema."
      do
        json ← AGen.genJson `Gen.suchThat` not A.isNull
        pure $ json `shouldViolate` JsonNullSchema

    generativeTestCase Long
      "No number JSON violates unrestricted number schema."
      do
        json ← A.fromNumber <$> Gen.chooseFloat bottom top
        pure $ json `shouldNotViolate` (JsonNumberSchema {})

    generativeTestCase Long
      "Any non-number JSON violates any number schema."
      do
        json ← AGen.genJson `Gen.suchThat` not A.isNumber
        schema ← JsonSchema.genNumberSchema
        pure $ json `shouldViolate` schema

    generativeTestCase Long
      "No object JSON violates unrestricted object schema."
      do
        json ←
          A.fromObject
            <$> Object.fromFoldable
            <$> genKeyValuePairs

        pure $ json `shouldNotViolate`
          (JsonObjectSchema { properties: Map.empty })

    generativeTestCase Long
      "Any non-object JSON violates any object schema."
      do
        json ← AGen.genJson `Gen.suchThat` not A.isObject
        schema ← JsonSchema.genObjectSchema
        pure $ json `shouldViolate` schema

    generativeTestCase Long
      "No string JSON violates unrestricted string schema."
      do
        json ← A.fromString <$> StringGen.genUnicodeString
        pure $ json `shouldNotViolate` (JsonStringSchema {})

    generativeTestCase Long
      "Any non-string JSON violates any string schema."
      do
        json ← AGen.genJson `Gen.suchThat` not A.isString
        schema ← JsonSchema.genStringSchema
        pure $ json `shouldViolate` schema

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
