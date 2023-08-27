module Docs.Printing
  ( Category(..)
  , formatAnchor
  , printTableOfContents
  , printCategory
  , renderCategory
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Markdown (CodeBlockType(..), Document, Node)
import Data.Markdown as M
import Data.Mermaid.FlowChart (FlowChartDef(..), Orientation(..))
import Data.Mermaid.FlowChart as FlowChart
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Docs.Types (Doc, PrintableExample, PrintableProperty)

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
  <<< String.replaceAll (Pattern ".") (Replacement "")
  <<< String.toLower

printCategory ∷ Category → Doc → Document
printCategory category doc =
  [ M.heading2 $ renderCategory category
  , M.paragraph $ printCategoryDescription category
  ]
    <>
      ( if Set.isEmpty doc.properties then []
        else
          [ M.paragraph "Properties:"
          ] <> printProperties doc.properties
      )
    <>
      ( if Set.isEmpty doc.examples then []
        else
          [ M.paragraph
              $ "Examples of " <> doc.computationDescription <> ":"
          ]
            <> printCategoryTableOfContents doc.examples
            <> foldMap printExample doc.examples
      )

printProperties ∷ Set PrintableProperty → Document
printProperties = Array.singleton
  <<< M.list
  <<< Set.map (M.paragraph <<< (_.title))

printCategoryTableOfContents ∷ Set PrintableExample → Document
printCategoryTableOfContents = Array.singleton
  <<< M.list
  <<< Set.map printCategoryTableOfContentsEntry

printCategoryTableOfContentsEntry ∷ ∀ r. { title ∷ String | r } → Node
printCategoryTableOfContentsEntry { title } = M.link title
  $ "#" <> formatAnchor title

printExample ∷ PrintableExample → Document
printExample { description, input, output, title } =
  [ M.rule
  , M.heading3 title
  , M.paragraph description
  , M.heading4 "Input"
  ]
    <> input
    <> [ M.heading4 "Output" ]
    <> output
