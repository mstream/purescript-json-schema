module JsonSchema.JsonPath
  ( JsonPath
  , JsonPathSegment(..)
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

type JsonPath = List JsonPathSegment

render ∷ JsonPath → NonEmptyString
render = foldl f (StringNE.nes (Proxy ∷ Proxy "$")) <<< List.reverse
  where
  f ∷ NonEmptyString → JsonPathSegment → NonEmptyString
  f acc = (acc <> _) <<< case _ of
    ItemIndex idx →
      StringNE.nes (Proxy ∷ Proxy "[") `StringNE.appendString` show idx
        <> StringNE.nes (Proxy ∷ Proxy "]")
    Property name →
      "/" `StringNE.prependString` name

data JsonPathSegment
  = ItemIndex Int
  | Property NonEmptyString

derive instance Eq JsonPathSegment
derive instance Generic JsonPathSegment _
derive instance Ord JsonPathSegment

instance Show JsonPathSegment where
  show = genericShow
