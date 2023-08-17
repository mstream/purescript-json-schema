module Test.Spec.JsonSchema.Codec.Printing (spec) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import JsonSchema (JsonSchema(..))
import JsonSchema as Schema
import JsonSchema.Codec.Printing as Printing
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (TestSpec)

spec ∷ TestSpec
spec = describe "Printing" do
  positiveTestCase
    { description: "an empty object schema"
    , expectedJson: A.jsonEmptyObject
    , schema: ObjectSchema Schema.defaultKeywords
    }

positiveTestCase
  ∷ { description ∷ String, expectedJson ∷ Json, schema ∷ JsonSchema }
  → TestSpec
positiveTestCase { description, expectedJson, schema } = it
  ("Should print " <> description)
  ( (A.stringify $ Printing.printSchema schema) `shouldEqual`
      A.stringify expectedJson
  )
