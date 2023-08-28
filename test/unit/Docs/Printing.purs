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
    , M.paragraph
        "Maintaining backward and forward compatibility is important for minimizing disruption and ensuring smooth transitions when updating JSON schemas."
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
    [ M.paragraph
        "Calculating JSON Schema Difference is a process used to identify the changes between two JSON schemata. It is used to to see what has been added, removed, or changed. This is useful for tracking changes over time, understanding the impact of changes, and managing versions of a schema. It can also be used to generate a diff report or to automate the process of updating dependent systems or documentation when a schema changes."
    ]
  Validation →
    [ M.paragraph
        "JSON validation is a specification for validating the structure and data types of JSON values. It allows you to specify the required properties, the types of values, the format of the data, and other constraints for a JSON object. This is useful for ensuring that the data received or sent in a JSON format is as expected and can be processed correctly. It helps to catch errors early, improve data quality, and reduce the amount of code needed for data validation."
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
