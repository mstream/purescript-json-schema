module Test.Snapshot.Spec.Markdown (spec) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as ArrayNE
import Data.Markdown (CodeLanguage(..), Document, FlowContentNode)
import Data.Markdown as M
import Data.Mermaid.FlowChart (FlowChartDef(..), Orientation(..))
import Data.Mermaid.FlowChart as FlowChart
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
  renderFixture <$>
    [ { document: longLinesExample
      , outputFile: StringNE.nes (Proxy ∷ Proxy "long-lines.md")
      }
    , { document: codeBlockExample
      , outputFile: StringNE.nes (Proxy ∷ Proxy "code-block.md")
      }
    , { document: mermaidFlowChartExample
      , outputFile: StringNE.nes (Proxy ∷ Proxy "mermaid-flow-chart.md")
      }
    ]

renderFixture
  ∷ { document ∷ DocumentExample, outputFile ∷ NonEmptyString }
  → Fixture DocumentExample
renderFixture { document, outputFile } = { input: document, outputPath }
  where
  outputPath ∷ NonEmptyString
  outputPath = StringNE.nes (Proxy ∷ Proxy "markdown/") <> outputFile

describeInput ∷ DocumentExample → String
describeInput { description } = "a markdown document featuring "
  <> description

renderMarkdownDocument ∷ DocumentExample → String
renderMarkdownDocument { document } = M.render
  { maxLineLength: 80 }
  document

longLinesExample ∷ DocumentExample
longLinesExample =
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

codeBlockExample ∷ DocumentExample
codeBlockExample =
  { description: "code block with an empty JSON inside it"
  , document
  }
  where
  document ∷ Document
  document = Array.singleton
    $ M.flowContent
    $ M.codeBlock Json [ "{}" ]

mermaidFlowChartExample ∷ DocumentExample
mermaidFlowChartExample =
  { description: "mermaid flow chart diagram"
  , document
  }
  where
  document ∷ Document
  document = Array.singleton
    $ M.flowContent
    $ M.renderMermaid flowChartDef

  flowChartDef ∷ FlowChartDef
  flowChartDef = FlowChartDef LeftToRight
    [ FlowChart.subGraph "subgraph1"
        [ FlowChart.box "subgraph1_box1" "box1"
        , FlowChart.box "subgraph1_box2" "box2"
        ]
    , FlowChart.box "box1" "box1"
    , FlowChart.normalArrow "subgraph1" "box1"
    , FlowChart.normalArrowWithAnnotation
        "arrow description"
        "subgraph1_box1"
        "subgraph1_box2"
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
