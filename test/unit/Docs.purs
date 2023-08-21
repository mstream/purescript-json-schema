module Test.Docs where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.FoldableWithIndex
  ( class FoldableWithIndex
  , foldMapWithIndex
  )
import Data.Map (Map)
import Data.Map as Map
import Data.Markdown (CodeBlockType(..), Document, Node)
import Data.Markdown as M
import Data.Mermaid.FlowChart (FlowChartDef(..), Orientation(..))
import Data.Mermaid.FlowChart as FlowChart
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.Spec.JsonSchema.Compatibility as Compatibility
import Test.Spec.JsonSchema.Diff as Diff
import Test.Spec.JsonSchema.Validation as Validation
import Test.Types (Example)

data Category
  = Compatibility
  | Diff
  | Validation

derive instance Eq Category
derive instance Ord Category

renderCategory ∷ Category → String
renderCategory = case _ of
  Compatibility →
    "JSON Schema Change Compatibility Checks"
  Diff →
    "JSON Schema Difference Calculation"
  Validation →
    "JSON Values Validation"

printCategoryDescription ∷ Category → String
printCategoryDescription = M.render <<< case _ of
  Compatibility →
    [ M.paragraph
        "**Backward compatibility** - an ability of a system to understand input intended for previous versions of itself"
    , M.paragraph
        "**Forward compatibility** - an ability of a system to understand input intended for future versions of itself"
    , M.paragraph
        "**Full compatibility** - backward and forward compatibility combined"
    , M.paragraph
        "**No compatibility** - neither level of compatibility"
    , renderMermaid $ FlowChartDef LeftToRight
        [ FlowChart.subGraph "data writers"
            [ FlowChart.capsule "current_writer" "writer"
            , FlowChart.capsule "next_writer" "writer<sub>+1</sub>"
            ]
        , FlowChart.subGraph "data readers"
            [ FlowChart.capsule "current_reader" "reader"
            , FlowChart.capsule "next_reader" "reader<sub>+1</sub>"
            ]
        , FlowChart.normalArrow
            "current_writer"
            "current_reader"
        , FlowChart.normalArrowWithAnnotation
            "backward compatibility"
            "current_writer"
            "next_reader"
        , FlowChart.normalArrowWithAnnotation
            "forward compatibility"
            "next_writer"
            "current_reader"
        ]
    ]
  Diff →
    [ M.paragraph "TODO"
    ]
  Validation →
    [ M.paragraph "TODO"
    ]

renderMermaid ∷ FlowChartDef → Node
renderMermaid flowChartDef =
  M.codeBlock Mermaid code
  where
  code ∷ String
  code = FlowChart.render flowChartDef

main ∷ Effect Unit
main = launchAff_
  $ FS.writeTextFile UTF8 "docs/src/examples/README.generated.md"
  $ M.render
  $ printExamples
  $ groupExamplesByCategory examples
  where
  examples ∷ Array PrintableExample
  examples =
    (makePrintable Compatibility <$> Compatibility.examples)
      <> (makePrintable Diff <$> Diff.examples)
      <> (makePrintable Validation <$> Validation.examples)

type PrintableExample =
  { category ∷ Category
  , description ∷ String
  , input ∷ Document
  , output ∷ Document
  , title ∷ String
  }

makePrintable ∷ ∀ i o. Category → Example i o → PrintableExample
makePrintable category example =
  { category
  , description: example.description
  , output: example.renderOutput example.expectedOutput
  , input: example.renderInput example.input
  , title: example.title
  }

groupExamplesByCategory
  ∷ ∀ f
  . Foldable f
  ⇒ f PrintableExample
  → Map Category (Array PrintableExample)
groupExamplesByCategory = foldl
  ( \acc example →
      Map.insertWith
        append
        example.category
        [ example ]
        acc
  )
  Map.empty

printExamples
  ∷ ∀ f
  . FoldableWithIndex Category f
  ⇒ f (Array PrintableExample)
  → Document
printExamples examplesByCategory =
  [ M.heading1 "Examples" ]
    <>
      ( printTableOfContents
          $ foldMapWithIndex
              (\category _ → [ category ])
              examplesByCategory
      )
    <> foldMapWithIndex printCategory examplesByCategory
  where
  printTableOfContents ∷ Array Category → Document
  printTableOfContents = Array.singleton
    <<< M.list
    <<< map printTableOfContentsEntry

  printTableOfContentsEntry ∷ Category → Node
  printTableOfContentsEntry category = M.link
    (renderCategory category)
    ("#" <> (formatAnchor $ renderCategory category))

  printCategory ∷ Category → Array PrintableExample → Document
  printCategory category examples =
    [ M.rule
    , M.heading2 $ renderCategory category
    , M.paragraph $ printCategoryDescription category
    ]
      <> foldMap printExample examples

  printExample ∷ PrintableExample → Document
  printExample { description, input, output, title } =
    [ M.heading3 $ "⌘ " <> title
    , M.paragraph description
    , M.heading4 "Input"
    ]
      <> input
      <> [ M.heading4 "Output" ]
      <> output

  formatAnchor ∷ String → String
  formatAnchor = String.replaceAll (Pattern " ") (Replacement "-")
    <<< String.toLower
