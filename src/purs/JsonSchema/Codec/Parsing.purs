module JsonSchema.Codec.Parsing (parseSchema) where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema

parseSchema ∷ Json → String \/ JsonSchema
parseSchema json = parseBooleanSchema json <|> parseObjectSchema json

parseBooleanSchema ∷ Json → String \/ JsonSchema
parseBooleanSchema json = do
  bool ← note
    (parsingErrorMessage "schema is not a JSON boolean")
    (A.toBoolean json)
  pure $ BooleanSchema bool

parseObjectSchema ∷ Json → String \/ JsonSchema
parseObjectSchema keywordsJson = do
  schemaObject ← note
    (parsingErrorMessage "schema is not a JSON object")
    (A.toObject keywordsJson)

  items ← maybe (Right Schema.defaultKeywords.items)
    (map Just <<< parseSchema)
    (Object.lookup "items" schemaObject)

  multipleOf ← parseMultipleOf schemaObject

  not ← maybe (Right Schema.defaultKeywords.not)
    (map Just <<< parseSchema)
    (Object.lookup "not" schemaObject)

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
    { items, multipleOf, not, required, typeKeyword, uniqueItems }

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
parseTypeKeywordSpec specJson = do
  typeJsons ← note "Types are not an array." $ A.toArray specJson
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
