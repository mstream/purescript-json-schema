module Docs.Main (main) where

import Prelude

import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map)
import Data.Map as Map
import Data.Markdown as M
import Data.Set (Set)
import Data.Tuple.Nested ((/\))
import Docs.Printing (Category(..))
import Docs.Printing as DocsPrinting
import Docs.Types (Doc)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.Spec.JsonSchema.Compatibility as Compatibility
import Test.Spec.JsonSchema.Diff as Diff
import Test.Spec.JsonSchema.Validation as Validation

main ∷ Effect Unit
main = launchAff_ $ saveDocumentation docByCategory
  where
  docByCategory ∷ Map Category Doc
  docByCategory = Map.fromFoldable
    [ Compatibility /\ Compatibility.doc
    , Diff /\ Diff.doc
    , Validation /\ Validation.doc
    ]

saveDocumentation ∷ Map Category Doc → Aff Unit
saveDocumentation specByCategory = do
  saveTableOfContents prefix $ Map.keys specByCategory
  traverseWithIndex_ (saveCategory prefix) specByCategory
  where
  prefix ∷ String
  prefix = "docs/src"

saveTableOfContents ∷ String → Set Category → Aff Unit
saveTableOfContents prefix =
  FS.writeTextFile UTF8 (prefix <> "/SUMMARY.md")
    <<< M.render
    <<< DocsPrinting.printTableOfContents

saveCategory ∷ String → Category → Doc → Aff Unit
saveCategory prefix category doc =
  FS.writeTextFile UTF8 path
    $ M.render
    $ DocsPrinting.printCategory category doc
  where
  path ∷ String
  path = prefix
    <> "/examples/"
    <>
      ( DocsPrinting.formatAnchor $ DocsPrinting.renderCategory
          category
      )
    <> ".md"
