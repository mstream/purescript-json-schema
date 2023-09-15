module Docs.Document (class Document, document) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.Markdown (FlowContentNode)
import Data.Markdown as M
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as SetNE

class Document a where
  document ∷ a → NonEmpty Array FlowContentNode

instance (Document a, Document e) ⇒ Document (e \/ a) where
  document = case _ of
    Left error →
      (M.paragraph $ ArrayNE.singleton $ M.text "an error:")
        :| (Array.fromFoldable $ document error)
    Right value →
      document value

instance (Document a) ⇒ Document (Array a) where
  document = documentFoldable true

instance (Document a) ⇒ Document (Set a) where
  document = documentFoldable false

documentFoldable
  ∷ ∀ a f
  . Document a
  ⇒ Foldable f
  ⇒ Boolean
  → f a
  → NonEmpty Array FlowContentNode
documentFoldable isOrdered = NE.singleton
  <<< maybe
    (M.paragraph $ ArrayNE.singleton $ M.text "∅")
    ( (if isOrdered then M.orderedList else M.unorderedList)
        <<< map (ArrayNE.fromNonEmpty <<< document)
    )
  <<< ArrayNE.fromFoldable

instance (Document a) ⇒ Document (NonEmptyArray a) where
  document = document <<< ArrayNE.toArray

instance (Document a) ⇒ Document (NonEmptySet a) where
  document = document <<< SetNE.toSet

instance Document String where
  document = NE.singleton
    <<< M.paragraph
    <<< ArrayNE.singleton
    <<< M.text

instance Document Boolean where
  document = document <<< show

instance Document Int where
  document = document <<< show
