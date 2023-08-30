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
import Node.FS.Perms as FSP
import Test.Spec.JsonSchema.Codec.Parsing as Parsing
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
    , Parsing /\ Parsing.doc
    , Validation /\ Validation.doc
    ]

saveDocumentation ∷ Map Category Doc → Aff Unit
saveDocumentation specByCategory = do
  FS.mkdir' (prefix <> "/" <> categoriesDirectory)
    { mode: FSP.mkPerms
        FSP.all
        (FSP.execute + FSP.read)
        (FSP.execute + FSP.read)
    , recursive: true
    }

  saveTableOfContents
    (prefix <> "/" <> "SUMMARY.md")
    categoryFilePath
    (Map.keys specByCategory)

  traverseWithIndex_
    ( \category doc →
        saveCategory
          (prefix <> "/" <> categoryFilePath category)
          category
          doc
    )
    specByCategory

  where
  prefix ∷ String
  prefix = "docs/src"

  categoryFilePath ∷ Category → String
  categoryFilePath category =
    categoriesDirectory
      <> "/"
      <>
        ( DocsPrinting.formatAnchor
            $ DocsPrinting.renderCategory category
        )
      <> ".md"

  categoriesDirectory ∷ String
  categoriesDirectory = "categories"

saveTableOfContents
  ∷ String → (Category → String) → Set Category → Aff Unit
saveTableOfContents path categoryFilePath =
  FS.writeTextFile UTF8 path
    <<< M.render
    <<< ([ M.heading1 "Summary" ] <> _)
    <<< DocsPrinting.printTableOfContents categoryFilePath

saveCategory ∷ String → Category → Doc → Aff Unit
saveCategory path category doc = do
  FS.writeTextFile UTF8 path
    $ M.render
    $ DocsPrinting.printCategory category doc
