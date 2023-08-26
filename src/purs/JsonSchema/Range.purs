module JsonSchema.Range (Boundary(..), Range, renderRange) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type Range = { from ∷ Boundary, to ∷ Boundary }

renderRange ∷ Range → String
renderRange range = renderFrom <> "," <> renderTo
  where
  renderFrom ∷ String
  renderFrom = case range.from of
    Closed x →
      "[" <> show x
    Open x →
      "(" <> show x

  renderTo ∷ String
  renderTo = case range.to of
    Closed x →
      show x <> "]"
    Open x →
      show x <> ")"

data Boundary = Closed Number | Open Number

derive instance Eq Boundary
derive instance Generic Boundary _
derive instance Ord Boundary

instance Show Boundary where
  show = genericShow
