module JsonSchema.SchemaPath
  ( SchemaPath
  , SchemaPathSegment(..)
  , render
  ) where

import Prelude

import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Type.Proxy (Proxy(..))

type SchemaPath = List SchemaPathSegment

render ∷ SchemaPath → NonEmptyString
render = foldl f (StringNE.nes (Proxy @"#")) <<< List.reverse
  where
  f ∷ NonEmptyString → SchemaPathSegment → NonEmptyString
  f acc = (acc `StringNE.appendString` "/" <> _) <<< case _ of
    ExclusiveMinimum →
      StringNE.nes (Proxy @"exclusiveMinimum")
    ExclusiveMaximum →
      StringNE.nes (Proxy @"exclusiveMaximum")
    Items →
      StringNE.nes (Proxy @"items")
    Maximum →
      StringNE.nes (Proxy @"maximum")
    Minimum →
      StringNE.nes (Proxy @"minimum")
    MultipleOf →
      StringNE.nes (Proxy @"multipleOf")
    Properties name →
      "properties/" `StringNE.prependString` name
    TypeKeyword →
      StringNE.nes (Proxy @"type")
    UniqueItems →
      StringNE.nes (Proxy @"uniqueItems")

data SchemaPathSegment
  = ExclusiveMaximum
  | ExclusiveMinimum
  | Items
  | Maximum
  | Minimum
  | MultipleOf
  | Properties NonEmptyString
  | TypeKeyword
  | UniqueItems

derive instance Eq SchemaPathSegment
derive instance Generic SchemaPathSegment _
derive instance Ord SchemaPathSegment

instance EncodeJson SchemaPathSegment where
  encodeJson = case _ of
    ExclusiveMinimum →
      A.fromString "exclusiveMinimum"
    ExclusiveMaximum →
      A.fromString "exclusiveMaximum"
    Items →
      A.fromString "items"
    Maximum →
      A.fromString "maximum"
    Minimum →
      A.fromString "minimum"
    MultipleOf →
      A.fromString "multipleOf"
    Properties name →
      A.fromString $ show name
    TypeKeyword →
      A.fromString "type"
    UniqueItems →
      A.fromString "uniqueItems"

instance Show SchemaPathSegment where
  show = genericShow
