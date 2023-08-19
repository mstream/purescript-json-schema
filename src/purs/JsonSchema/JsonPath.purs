module JsonSchema.JsonPath
  ( JsonPath
  , JsonPathSegment(..)
  , render
  ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Show.Generic (genericShow)

type JsonPath = List JsonPathSegment

render ∷ JsonPath → String
render = ("$" <> _) <<< foldMap f <<< List.reverse
  where
  f ∷ JsonPathSegment → String
  f = case _ of
    ItemIndex idx →
      "[" <> show idx <> "]"
    Property name →
      "/" <> name

data JsonPathSegment
  = ItemIndex Int
  | Property String

derive instance Eq JsonPathSegment
derive instance Generic JsonPathSegment _
derive instance Ord JsonPathSegment

instance Show JsonPathSegment where
  show = genericShow
