module JsonSchema.SchemaPath
  ( SchemaPath
  , SchemaPathSegment(..)
  , render
  ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Show.Generic (genericShow)

type SchemaPath = List SchemaPathSegment

render ∷ SchemaPath → String
render = ("#" <> _) <<< foldMap f <<< List.reverse
  where
  f ∷ SchemaPathSegment → String
  f = case _ of
    Items →
      "/items"
    TypeKeyword →
      "/type"
    UniqueItems →
      "/uniqueItems"

data SchemaPathSegment = Items | TypeKeyword | UniqueItems

derive instance Eq SchemaPathSegment
derive instance Generic SchemaPathSegment _
derive instance Ord SchemaPathSegment

instance Show SchemaPathSegment where
  show = genericShow
