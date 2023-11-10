module JsonSchema.Range (Boundary(..), Range, renderRange) where

import Prelude

import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Markdown (PhrasingContentNode)
import Data.Markdown as M
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Type.Proxy (Proxy(..))

type Range = { from ∷ Boundary, to ∷ Boundary }

renderRange ∷ Range → PhrasingContentNode
renderRange range = M.inlineCode
  $ renderFrom <> StringNE.nes (Proxy ∷ Proxy ",") <> renderTo
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

instance EncodeJson Boundary where
  encodeJson = case _ of
    Closed x →
      A.fromString $ show x <> " (inclusively)"
    Open x →
      A.fromString $ show x <> " (exclusively)"

instance Show Boundary where
  show = genericShow
