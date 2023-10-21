module Test.Snapshot.Spec.Markdown (spec) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as ArrayNE
import Data.Markdown (Document, FlowContentNode)
import Data.Markdown as M
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Test.Snapshot.Utils (Fixture, SnapshotTestSpec)
import Type.Proxy (Proxy(..))

type DocumentExample = { description ∷ String, document ∷ Document }

spec ∷ SnapshotTestSpec DocumentExample
spec =
  { describeInput
  , description: StringNE.nes (Proxy ∷ Proxy "abc")
  , executeCommand: pure <<< renderMarkdownDocument
  , fixtures
  }

fixtures ∷ Array (Fixture DocumentExample)
fixtures =
  [ { input: documentExample1
    , outputPath: "markdown/document1.md"
    }
  ]

describeInput ∷ DocumentExample → String
describeInput { description } = "a markdown document featuring "
  <> description

renderMarkdownDocument ∷ DocumentExample → String
renderMarkdownDocument { document } = M.render
  { maxLineLength: 80 }
  document

documentExample1 ∷ DocumentExample
documentExample1 =
  { description: "very long text lines"
  , document
  }
  where
  document ∷ Document
  document = M.flowContent
    <$>
      [ paragraphWithLineOfLengthTenTimes 2
      , paragraphWithLineOfLengthTenTimes 4
      , paragraphWithLineOfLengthTenTimes 6
      , paragraphWithLineOfLengthTenTimes 8
      , paragraphWithLineOfLengthTenTimes 10
      , paragraphWithLineOfLengthTenTimes 12
      , paragraphWithLineOfLengthTenTimes 14
      , paragraphWithLineOfLengthTenTimes 16
      , paragraphWithLineOfLengthTenTimes 18
      , paragraphWithLineOfLengthTenTimes 20
      ]

paragraphWithLineOfLengthTenTimes ∷ Int → FlowContentNode
paragraphWithLineOfLengthTenTimes n = M.paragraph
  $ M.text description `ArrayNE.cons'` [ M.lineBreak, M.text line ]
  where
  description ∷ NonEmptyString
  description = StringNE.nes (Proxy ∷ Proxy "The following line is ")
    `StringNE.appendString` (show (10 * n) <> " characters long.")

  line ∷ NonEmptyString
  line = linePrefix `StringNE.appendString` lineSuffix

  linePrefix ∷ NonEmptyString
  linePrefix = StringNE.nes (Proxy ∷ Proxy "aaaaaaaaa ")

  lineSuffix ∷ String
  lineSuffix =
    String.joinWith " "
      (Array.replicate (n - 1) (stringOfLength 9))
      <> "$"

stringOfLength ∷ Int → String
stringOfLength n = String.joinWith "" (Array.replicate n "a")
