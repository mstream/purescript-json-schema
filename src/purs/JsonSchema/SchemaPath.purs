module JsonSchema.SchemaPath
  ( SchemaPath
  , SchemaPathSegment(..)
  , render
  ) where

import Prelude

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
render = foldl f (StringNE.nes (Proxy ∷ Proxy "#")) <<< List.reverse
  where
  f ∷ NonEmptyString → SchemaPathSegment → NonEmptyString
  f acc = (acc `StringNE.appendString` "/" <> _) <<< case _ of
    ExclusiveMinimum →
      StringNE.nes (Proxy ∷ Proxy "exclusiveMinimum")
    ExclusiveMaximum →
      StringNE.nes (Proxy ∷ Proxy "exclusiveMaximum")
    Items →
      StringNE.nes (Proxy ∷ Proxy "items")
    Maximum →
      StringNE.nes (Proxy ∷ Proxy "maximum")
    Minimum →
      StringNE.nes (Proxy ∷ Proxy "minimum")
    MultipleOf →
      StringNE.nes (Proxy ∷ Proxy "multipleOf")
    Properties name →
      "properties/" `StringNE.prependString` name
    TypeKeyword →
      StringNE.nes (Proxy ∷ Proxy "type")
    UniqueItems →
      StringNE.nes (Proxy ∷ Proxy "uniqueItems")

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

instance Show SchemaPathSegment where
  show = genericShow
