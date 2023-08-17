module JsonSchema.Codec.Printing (printSchema) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object as Object
import JsonSchema (JsonSchema(..), JsonValueType(..), Keywords)

printSchema ∷ JsonSchema → Json
printSchema = case _ of
  BooleanSchema bool →
    A.fromBoolean bool
  ObjectSchema keywords →
    printObjectSchema keywords

printObjectSchema ∷ Keywords → Json
printObjectSchema keywords = A.fromObject
  $ Object.fromFoldable
  $
    printItems keywords.items
      <> printNot keywords.not
      <> printRequired keywords.required
      <> printTypeKeyword keywords.typeKeyword
      <> printUniqueItems keywords.uniqueItems
  where
  printItems ∷ Maybe JsonSchema → Array (String /\ Json)
  printItems mbSchema = maybe []
    (Array.singleton <<< ("items" /\ _) <<< printSchema)
    mbSchema

  printNot ∷ Maybe JsonSchema → Array (String /\ Json)
  printNot mbSchema = maybe []
    (Array.singleton <<< ("not" /\ _) <<< printSchema)
    mbSchema

printRequired ∷ Set String → Array (String /\ Json)
printRequired propertyNames =
  if Set.isEmpty propertyNames then []
  else
    [ "required" /\ printRequiredKeywordSpec propertyNames ]

printTypeKeyword ∷ Maybe (Set JsonValueType) → Array (String /\ Json)
printTypeKeyword = case _ of
  Just jsonValueTypes →
    [ "type" /\ printTypeKeywordSpec jsonValueTypes ]

  Nothing →
    []

printUniqueItems ∷ Boolean → Array (String /\ Json)
printUniqueItems bool =
  if bool then [ "uniqueItems" /\ A.jsonTrue ] else []

printTypeKeywordSpec ∷ Set JsonValueType → Json
printTypeKeywordSpec = A.fromArray
  <<< map printJsonValueType
  <<< Set.toUnfoldable

printRequiredKeywordSpec ∷ Set String → Json
printRequiredKeywordSpec = A.fromArray
  <<< map A.fromString
  <<< Set.toUnfoldable

printJsonValueType ∷ JsonValueType → Json
printJsonValueType = A.fromString <<< case _ of
  JsonArray →
    "array"
  JsonBoolean →
    "boolean"
  JsonInteger →
    "integer"
  JsonNull →
    "null"
  JsonNumber →
    "number"
  JsonObject →
    "object"
  JsonString →
    "string"
