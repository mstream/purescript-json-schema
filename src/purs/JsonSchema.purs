module JsonSchema
  ( JsonSchema(..)
  , JsonValueType(..)
  , Keywords
  , defaultKeywords
  , renderJsonValueType
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)

data JsonSchema
  = BooleanSchema Boolean
  | ObjectSchema Keywords

derive instance Eq JsonSchema
derive instance Generic JsonSchema _
derive instance Ord JsonSchema

instance Show JsonSchema where
  show schema = genericShow schema

type Keywords =
  { items ∷ Maybe JsonSchema
  , multipleOf ∷ Maybe Number
  , not ∷ Maybe JsonSchema
  , required ∷ Set String
  , typeKeyword ∷ Maybe (Set JsonValueType)
  , uniqueItems ∷ Boolean
  }

defaultKeywords ∷ Keywords
defaultKeywords =
  { items: Nothing
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

instance Show JsonValueType where
  show keyword = genericShow keyword

renderJsonValueType ∷ JsonValueType → String
renderJsonValueType = case _ of
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
