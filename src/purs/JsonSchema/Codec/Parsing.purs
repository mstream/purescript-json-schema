module JsonSchema.Codec.Parsing (parseSchema) where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema
import JsonValue (JsonValue)

parseSchema ∷ JsonValue → String \/ JsonSchema
parseSchema json = parseBooleanSchema json
  <|> parseObjectSchema json
  <|> Left "the JSON value is neither a boolean nor an object"

parseBooleanSchema ∷ JsonValue → String \/ JsonSchema
parseBooleanSchema json = do
  bool ← note
    (parsingErrorMessage "the JSON value is not a JSON boolean")
    (A.toBoolean $ unwrap json)
  pure $ BooleanSchema bool

parseObjectSchema ∷ JsonValue → String \/ JsonSchema
parseObjectSchema keywordsJson = do
  schemaObject ← note
    (parsingErrorMessage "the JSON value is not a JSON object")
    (A.toObject $ unwrap keywordsJson)

  exclusiveMaximum ← parseOptionalNumber
    "exclusiveMaximum"
    schemaObject

  exclusiveMinimum ← parseOptionalNumber
    "exclusiveMinimum"
    schemaObject

  items ← maybe (Right Schema.defaultKeywords.items)
    (map Just <<< parseSchema)
    (wrap <$> Object.lookup "items" schemaObject)

  maximum ← parseOptionalNumber
    "maximum"
    schemaObject

  minimum ← parseOptionalNumber
    "minimum"
    schemaObject

  multipleOf ← parseMultipleOf schemaObject

  not ← maybe (Right Schema.defaultKeywords.not)
    (map Just <<< parseSchema)
    (wrap <$> Object.lookup "not" schemaObject)

  required ← maybe (Right Schema.defaultKeywords.required)
    parseRequiredKeywordSpec
    (Object.lookup "required" schemaObject)

  typeKeyword ← case Object.lookup "type" schemaObject of
    Just typeKeywordSpecJson →
      Just <$> parseTypeKeywordSpec typeKeywordSpecJson
    Nothing →
      Right Schema.defaultKeywords.typeKeyword

  uniqueItems ← maybe (Right Schema.defaultKeywords.uniqueItems)
    ( \json →
        if A.isNull json then Right Schema.defaultKeywords.uniqueItems
        else note "Unique items is not a boolean." $ A.toBoolean json
    )
    (Object.lookup "uniqueItems" schemaObject)

  pure $ ObjectSchema
    { exclusiveMaximum
    , exclusiveMinimum
    , items
    , maximum
    , minimum
    , multipleOf
    , not
    , required
    , typeKeyword
    , uniqueItems
    }

parseOptionalNumber ∷ String → Object Json → String \/ Maybe Number
parseOptionalNumber name = Object.lookup name >>> case _ of
  Just json →
    case A.toNumber json of
      Just x →
        Right $ Just x
      Nothing →
        Left $ name <> " is not a number."
  Nothing →
    Right Nothing

parseMultipleOf ∷ Object Json → String \/ Maybe Number
parseMultipleOf = Object.lookup "multipleOf" >>> case _ of
  Just json →
    if A.isNull json then Right default
    else case A.toNumber json of
      Just x →
        if x > zero then Right $ Just x
        else Left "multipleOf must be greater than zero."
      Nothing →
        Left "multipleOf is not a number."
  Nothing →
    Right default
  where
  default ∷ Maybe Number
  default = Schema.defaultKeywords.multipleOf

parseTypeKeywordSpec ∷ Json → String \/ Set JsonValueType
parseTypeKeywordSpec specJson =
  parseStringSpec specJson <|> parseArraySpec specJson
  where
  parseStringSpec ∷ Json → String \/ Set JsonValueType
  parseStringSpec = map Set.singleton <<< parseJsonValueType

  parseArraySpec ∷ Json → String \/ Set JsonValueType
  parseArraySpec json = do
    typeJsons ← note "Types are not an array." $ A.toArray json
    types ← traverse parseJsonValueType typeJsons
    pure $ Set.fromFoldable types

parseRequiredKeywordSpec ∷ Json → String \/ Set String
parseRequiredKeywordSpec specJson = do
  propertyNameJsons ← note "Property names are not an array."
    $ A.toArray specJson
  propertyNames ← traverse
    (note "Property name is not a string." <<< A.toString)
    propertyNameJsons
  pure $ Set.fromFoldable propertyNames

parseJsonValueType ∷ Json → String \/ JsonValueType
parseJsonValueType json = case A.toString json of
  Just "array" →
    Right JsonArray

  Just "boolean" →
    Right JsonBoolean

  Just "integer" →
    Right JsonInteger

  Just "null" →
    Right JsonNull

  Just "number" →
    Right JsonNumber

  Just "object" →
    Right JsonObject

  Just "string" →
    Right JsonString

  Just otherJsonValueType →
    Left $ "Unsupported JSON value type: " <> otherJsonValueType

  Nothing →
    Left "JSON value type is not a string"

parsingErrorMessage ∷ String → String
parsingErrorMessage reason = "Invalid schema: " <> reason
