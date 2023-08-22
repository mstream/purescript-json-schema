module Test.Docs where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, traverse_)
import Data.FoldableWithIndex
  ( class FoldableWithIndex
  , foldMapWithIndex
  , traverseWithIndex_
  )
import Data.Map (Map)
import Data.Map as Map
import Data.Markdown (CodeBlockType(..), Document, Node)
import Data.Markdown as M
import Data.Mermaid.FlowChart (FlowChartDef(..), Orientation(..))
import Data.Mermaid.FlowChart as FlowChart
import Data.Set (Set)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
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
  $ saveExamples
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

saveExamples ∷ Map Category (Array PrintableExample) → Aff Unit
saveExamples examplesByCategory = do
  saveTableOfContents prefix $ Map.keys examplesByCategory
  traverseWithIndex_ (saveCategory prefix) examplesByCategory
  where
  prefix ∷ String
  prefix = "docs/src"

saveTableOfContents ∷ String → Set Category → Aff Unit
saveTableOfContents prefix =
  FS.writeTextFile UTF8 (prefix <> "/SUMMARY.md")
    <<< M.render
    <<< printTableOfContents

saveCategory
  ∷ String
  → Category
  → Array PrintableExample
  → Aff Unit
saveCategory prefix category examples =
  FS.writeTextFile UTF8 path
    $ M.render
    $ printCategory category examples
  where
  path ∷ String
  path = prefix
    <> "/examples/"
    <> (formatAnchor $ renderCategory category)
    <> ".md"

printTableOfContents ∷ Set Category → Document
printTableOfContents = foldMap f
  where
  f ∷ Category → Document
  f = Array.singleton
    <<< M.list
    <<< Array.singleton
    <<< printTableOfContentsEntry

printTableOfContentsEntry ∷ Category → Node
printTableOfContentsEntry category = M.link
  (renderCategory category)
  ((formatAnchor $ "examples/" <> renderCategory category) <> ".md")

formatAnchor ∷ String → String
formatAnchor = String.replaceAll (Pattern " ") (Replacement "-")
  <<< String.toLower

printCategory ∷ Category → Array PrintableExample → Document
printCategory category examples =
  [ M.rule
  , M.heading2 $ renderCategory category
  , M.paragraph $ printCategoryDescription category
  , M.paragraph "Examples:"
  ]
    <> printCategoryTableOfContents examples
    <> foldMap printExample examples

printCategoryTableOfContents ∷ Array PrintableExample → Document
printCategoryTableOfContents = foldMap f
  where
  f ∷ PrintableExample → Document
  f = Array.singleton
    <<< M.list
    <<< Array.singleton
    <<< printCategoryTableOfContentsEntry

printCategoryTableOfContentsEntry ∷ PrintableExample → Node
printCategoryTableOfContentsEntry { title } = M.link title
  $ "#" <> formatAnchor title

printExample ∷ PrintableExample → Document
printExample { description, input, output, title } =
  [ M.heading3 title
  , M.paragraph description
  , M.heading4 "Input"
  ]
    <> input
    <> [ M.heading4 "Output" ]
    <> output
