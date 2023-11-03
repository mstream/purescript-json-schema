module JsonSchema
  ( JsonSchema(..)
  , JsonValueType(..)
  , Keywords
  , defaultKeywords
  , print
  , renderJsonValueType
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Tuple.Nested (type (/\), (/\))
import Docs.Document (class Document, document)
import Foreign.Object as Object
import JsonValue (JsonValue)
import Type.Proxy (Proxy(..))

data JsonSchema
  = BooleanSchema Boolean
  | ObjectSchema Keywords

derive instance Eq JsonSchema
derive instance Generic JsonSchema _
derive instance Ord JsonSchema

instance Show JsonSchema where
  show schema = genericShow schema

instance Document JsonSchema where
  document = document <<< print

type Keywords =
  { exclusiveMaximum ∷ Maybe Number
  , exclusiveMinimum ∷ Maybe Number
  , items ∷ Maybe JsonSchema
  , maximum ∷ Maybe Number
  , minimum ∷ Maybe Number
  , multipleOf ∷ Maybe Number
  , not ∷ Maybe JsonSchema
  , required ∷ Set String
  , typeKeyword ∷ Maybe (Set JsonValueType)
  , uniqueItems ∷ Boolean
  }

defaultKeywords ∷ Keywords
defaultKeywords =
  { exclusiveMaximum: Nothing
  , exclusiveMinimum: Nothing
  , items: Nothing
  , maximum: Nothing
  , minimum: Nothing
  , multipleOf: Nothing
  , not: Nothing
  , required: Set.empty
  , typeKeyword: Nothing
  , uniqueItems: false
  }

data JsonValueType
  = JsonArray
  | JsonBoolean
  | JsonInteger
  | JsonNull
  | JsonNumber
  | JsonObject
  | JsonString

derive instance Eq JsonValueType
derive instance Generic JsonValueType _
derive instance Ord JsonValueType

instance EncodeJson JsonValueType where
  encodeJson = A.fromString
    <<< StringNE.toString
    <<< renderJsonValueType

instance Show JsonValueType where
  show keyword = genericShow keyword

renderJsonValueType ∷ JsonValueType → NonEmptyString
renderJsonValueType = case _ of
  JsonArray →
    StringNE.nes (Proxy ∷ Proxy "array")
  JsonBoolean →
    StringNE.nes (Proxy ∷ Proxy "boolean")
  JsonInteger →
    StringNE.nes (Proxy ∷ Proxy "integer")
  JsonNull →
    StringNE.nes (Proxy ∷ Proxy "null")
  JsonNumber →
    StringNE.nes (Proxy ∷ Proxy "number")
  JsonObject →
    StringNE.nes (Proxy ∷ Proxy "object")
  JsonString →
    StringNE.nes (Proxy ∷ Proxy "string")

print ∷ JsonSchema → JsonValue
print = case _ of
  BooleanSchema bool →
    wrap $ A.fromBoolean bool
  ObjectSchema keywords →
    printObjectSchema keywords

printObjectSchema ∷ Keywords → JsonValue
printObjectSchema keywords = wrap
  $ A.fromObject
  $ Object.fromFoldable
  $ (map unwrap) <$> printKeywords
  where
  printKeywords ∷ Array (String /\ JsonValue)
  printKeywords =
    printOptionalNumber "exclusiveMaximum" keywords.exclusiveMaximum
      <> printOptionalNumber "exclusiveMinimum"
        keywords.exclusiveMinimum
      <> printItems keywords.items
      <> printOptionalNumber "maximum" keywords.maximum
      <> printOptionalNumber "minimum" keywords.minimum
      <> printMultipleOf keywords.multipleOf
      <> printNot keywords.not
      <> printRequired keywords.required
      <> printTypeKeyword keywords.typeKeyword
      <> printUniqueItems keywords.uniqueItems

  printItems ∷ Maybe JsonSchema → Array (String /\ JsonValue)
  printItems mbSchema = maybe []
    (Array.singleton <<< ("items" /\ _) <<< print)
    mbSchema

  printNot ∷ Maybe JsonSchema → Array (String /\ JsonValue)
  printNot mbSchema = maybe []
    (Array.singleton <<< ("not" /\ _) <<< print)
    mbSchema

printOptionalNumber
  ∷ String → Maybe Number → Array (String /\ JsonValue)
printOptionalNumber name = case _ of
  Just x →
    [ name /\ (wrap $ A.fromNumber x) ]
  Nothing → []

printMultipleOf ∷ Maybe Number → Array (String /\ JsonValue)
printMultipleOf = case _ of
  Just x →
    [ "multipleOf" /\ (wrap $ A.fromNumber x) ]
  Nothing → []

printRequired ∷ Set String → Array (String /\ JsonValue)
printRequired propertyNames =
  if Set.isEmpty propertyNames then []
  else
    [ "required" /\ printRequiredKeywordSpec propertyNames ]

printTypeKeyword
  ∷ Maybe (Set JsonValueType) → Array (String /\ JsonValue)
printTypeKeyword = case _ of
  Just jsonValueTypes →
    [ "type" /\ printTypeKeywordSpec jsonValueTypes ]

  Nothing →
    []

printUniqueItems ∷ Boolean → Array (String /\ JsonValue)
printUniqueItems bool =
  if bool == defaultKeywords.uniqueItems then []
  else [ "uniqueItems" /\ (wrap $ A.fromBoolean bool) ]

printTypeKeywordSpec ∷ Set JsonValueType → JsonValue
printTypeKeywordSpec = wrap
  <<< A.fromArray
  <<< map printJsonValueType
  <<< Set.toUnfoldable

printRequiredKeywordSpec ∷ Set String → JsonValue
printRequiredKeywordSpec = wrap
  <<< A.fromArray
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
