module Test.Spec.JsonSchema.Codec (spec) where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Gen as AGen
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (notElem)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Gen as StringGen
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import JsonSchema (JsonSchema)
import JsonSchema as Schema
import JsonSchema.Codec as Codec
import Test.QuickCheck (Result(..), (===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

data InvalidSchemaJsonSpec
  = HasUnsupportedType
  | NotAnObject

genInvalidSchemaJson
  ∷ ∀ m
  . Lazy (m Json)
  ⇒ MonadGen m
  ⇒ MonadRec m
  ⇒ InvalidSchemaJsonSpec
  → m Json
genInvalidSchemaJson = case _ of
  HasUnsupportedType → do
    typeName ← StringGen.genUnicodeString `Gen.suchThat`
      ( \s → s `notElem`
          [ "array"
          , "boolean"
          , "integer"
          , "null"
          , "number"
          , "object"
          , "string"
          ]
      )
    pure $ A.fromObject $ Object.fromFoldable
      [ "type" /\ A.fromString typeName
      ]
  NotAnObject →
    AGen.genJson `Gen.suchThat` (not A.isObject)

spec ∷ TestSpec
spec = describe "Codec" do

  describe "parseSchema and printSchema" do

    generativeTestCase Long
      "Schemata do not change after being printed and parsed back."
      do
        schema ← Schema.genSchema
        let
          printed = Codec.printSchema schema
          parsingResult = Codec.parseSchema printed
        pure case parsingResult of
          Left errorMessage →
            Failed $ show
              { errorMessage
              , expectedSchema: schema
              , printedJson: A.stringify printed
              }
          Right parsedSchema →
            parsedSchema === schema

  describe "parseSchema" do

    generativeTestCase Long
      "Schemata not being a JSON object are not parsable"
      do
        invalidSchemaJson ← genInvalidSchemaJson NotAnObject
        pure $ errorMessageShouldMention
          { description: "an object"
          , parsingResult: Codec.parseSchema invalidSchemaJson
          , pattern: "object"
          }

    generativeTestCase Long
      "Schemata not having a supported type are not parsable"
      do
        invalidSchemaJson ← genInvalidSchemaJson HasUnsupportedType
        pure $ errorMessageShouldMention
          { description: "a type"
          , parsingResult: Codec.parseSchema invalidSchemaJson
          , pattern: "type"
          }

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
