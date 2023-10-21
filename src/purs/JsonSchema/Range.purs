module JsonSchema.Range (Boundary(..), Range, renderRange) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Type.Proxy (Proxy(..))

type Range = { from ∷ Boundary, to ∷ Boundary }

renderRange ∷ Range → NonEmptyString
renderRange range = renderFrom
  <> StringNE.nes (Proxy ∷ Proxy ",")
  <> renderTo
  where
  renderFrom ∷ NonEmptyString
  renderFrom = case range.from of
    Closed x →
      StringNE.nes (Proxy ∷ Proxy "[") `StringNE.appendString` show x
    Open x →
      StringNE.nes (Proxy ∷ Proxy "(") `StringNE.appendString` show x

  renderTo ∷ NonEmptyString
  renderTo = case range.to of
    Closed x →
      show x `StringNE.prependString` StringNE.nes (Proxy ∷ Proxy "]")
    Open x →
      show x `StringNE.prependString` StringNE.nes (Proxy ∷ Proxy ")")

data Boundary = Closed Number | Open Number

derive instance Eq Boundary
derive instance Generic Boundary _
derive instance Ord Boundary

instance Show Boundary where
  show = genericShow
