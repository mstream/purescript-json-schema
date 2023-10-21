module Docs.Main (main) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Markdown (Document, FormattingOptions, PhrasingContentNode)
import Data.Markdown as M
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as SetNE
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Traversable (traverse_)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Docs.Utils (documentComputation)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms as FSP
import Test.Unit.Spec.JsonSchema (spec) as Schema
import Test.Unit.Spec.JsonSchema.Codec.Parsing as Parsing
import Test.Unit.Spec.JsonSchema.Compatibility as Compatibility
import Test.Unit.Spec.JsonSchema.Difference as Difference
import Test.Unit.Spec.JsonSchema.Validation as Validation
import Type.Proxy (Proxy(..))

main ∷ Effect Unit
main = launchAff_ $ saveDocumentation docsByTitle
  where
  docsByTitle ∷ NonEmptySet (NonEmptyString /\ Document)
  docsByTitle = SetNE.fromFoldable1
    $ (documentComputation Parsing.spec)
        `ArrayNE.cons'`
          [ documentComputation Compatibility.spec
          , documentComputation Difference.spec
          , documentComputation Schema.spec
          , documentComputation Validation.spec
          ]

buildTableOfContents ∷ NonEmptySet NonEmptyString → Document
buildTableOfContents titles =
  M.document
    [ M.heading1
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes (Proxy ∷ Proxy "Summary")
    , M.orderedList $
        ( ArrayNE.singleton
            <<< M.paragraph
            <<< ArrayNE.singleton
            <<< titleToLink
        )
          <$> (ArrayNE.fromFoldable1 titles)
    ]

titleToLink ∷ NonEmptyString → PhrasingContentNode
titleToLink title = M.link title
  (M.urlOfLocalFile $ documentFilePath title)

documentFilePath ∷ NonEmptyString → NonEmptyString
documentFilePath title =
  (M.headingIdToString $ M.formatAnchor title)
    `StringNE.prependString` (StringNE.nes (Proxy ∷ Proxy ".md"))

saveDocumentation ∷ NonEmptySet (NonEmptyString /\ Document) → Aff Unit
saveDocumentation docsByTitle = do
  FS.mkdir' prefix
    { mode: FSP.mkPerms
        FSP.all
        (FSP.execute + FSP.read)
        (FSP.execute + FSP.read)
    , recursive: true
    }

  FS.writeTextFile UTF8
    (prefix <> "/SUMMARY.md")
    ( M.render formattingOptions
        $ buildTableOfContents
        $ Tuple.fst `SetNE.map` docsByTitle
    )

  traverse_
    ( \(title /\ doc) →
        FS.writeTextFile
          UTF8
          ( prefix
              <> "/"
              <> (StringNE.toString $ documentFilePath title)
          )
          (M.render formattingOptions doc)
    )
    docsByTitle

  where
  formattingOptions ∷ FormattingOptions
  formattingOptions = { maxLineLength: 72 }

  prefix ∷ String
  prefix = "docs/src"
