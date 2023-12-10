module Computation.Utils
  ( DocumentInput
  , documentComputation
  ) where

import Prelude

import Computation
  ( ComputationContext
  , ComputationDescription
  , ComputationExample
  , ComputationProperty
  , GetValueDescription(..)
  , GetValueSample(..)
  , ToValueSpec(..)
  , ValueSample
  , ValueSpec(..)
  )
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Markdown (Document, FlowContentNode)
import Data.Markdown as M
import Data.Maybe (Maybe(..), maybe)
import Data.Mermaid.FlowChart
  ( FlowChartDef(..)
  , Orientation(..)
  , Segment
  )
import Data.Mermaid.FlowChart as FlowChart
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Docs.Document (class Document, document)
import Heterogeneous.Folding
  ( class Folding
  , class FoldlRecord
  , ConstFolding
  , hfoldl
  )
import Heterogeneous.Mapping
  ( class MapRecordWithIndex
  , class Mapping
  , ConstMapping
  , hmap
  , mapping
  )
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))

data DocumentInput = DocumentInput

instance
  ( Document a
  ) ⇒
  Folding
    DocumentInput
    (NonEmptyArray FlowContentNode)
    (ValueSample a)
    (NonEmptyArray FlowContentNode) where
  folding DocumentInput acc = (acc <> _)
    <<< ArrayNE.fromNonEmpty
    <<< document

instance
  Folding
    DocumentInput
    (Array NonEmptyString)
    (ValueSpec a)
    (Array NonEmptyString) where
  folding DocumentInput acc =
    (acc <> _) <<< (\(ValueSpec desc) → [ desc ])

documentSchema
  ∷ ∀ isp o rlsp
  . FoldlRecord
      (ConstFolding DocumentInput)
      (Array NonEmptyString)
      rlsp
      isp
      (Array NonEmptyString)
  ⇒ RowToList isp rlsp
  ⇒ { | isp }
  → ValueSpec o
  → NonEmptyArray FlowContentNode
documentSchema inputSpecs (ValueSpec outputDesc) =
  ArrayNE.singleton $ M.renderMermaid flowChartDef
  where
  flowChartDef ∷ FlowChartDef
  flowChartDef = FlowChartDef LeftToRight
    [ FlowChart.subGraph "inputs" inputBoxes
    , FlowChart.subGraph "output"
        [ FlowChart.box "output_desc" (StringNE.toString outputDesc)
        ]
    , FlowChart.normalArrow "inputs" "output"
    ]

  inputBoxes ∷ Array Segment
  inputBoxes =
    ( \inputIdx inputDesc → FlowChart.box
        ("input_desc_" <> show inputIdx)
        (StringNE.toString inputDesc)
    )
      `mapWithIndex` inputDescriptions

  inputDescriptions ∷ Array NonEmptyString
  inputDescriptions = hfoldl
    DocumentInput
    (mempty ∷ Array NonEmptyString)
    inputSpecs

documentInput
  ∷ ∀ isa rlsa
  . FoldlRecord
      (ConstFolding DocumentInput)
      (NonEmptyArray FlowContentNode)
      rlsa
      isa
      (NonEmptyArray FlowContentNode)
  ⇒ RowToList isa rlsa
  ⇒ { | isa }
  → NonEmptyArray FlowContentNode
documentInput = hfoldl
  DocumentInput
  ( ArrayNE.singleton
      $ M.paragraph
      $ ArrayNE.singleton
      $ M.emphasis
      $ ArrayNE.singleton
      $ M.text
      $ StringNE.nes (Proxy ∷ Proxy "Input:")
  )

documentOutput
  ∷ ∀ osa o
  . Document o
  ⇒ Mapping GetValueSample osa o
  ⇒ Mapping GetValueDescription osa NonEmptyString
  ⇒ osa
  → NonEmptyArray FlowContentNode
documentOutput outputSample =
  ( ArrayNE.singleton
      $ M.paragraph
      $ ArrayNE.singleton
      $ M.emphasis
      $ ArrayNE.singleton
      $ M.text
      $ StringNE.nes (Proxy ∷ Proxy "Output:")
  )
    <>
      ( ArrayNE.singleton
          $ M.paragraph
          $ ArrayNE.singleton
          $ M.text
          $ mapping GetValueDescription outputSample
              <> StringNE.nes (Proxy ∷ Proxy ":")
      )
    <>
      ( ArrayNE.singleton $ M.blockquote $ document
          (mapping GetValueSample outputSample)
      )

type ComputationDocsSpec isa isp o osa r =
  { context ∷ ComputationContext
  , description ∷ ComputationDescription isp o
  , examples ∷ Array (ComputationExample isa osa)
  , input ∷ { | isp }
  , output ∷ ValueSpec o
  , properties ∷ Array (ComputationProperty isa o)
  | r
  }

documentComputation
  ∷ ∀ i isa isp o osa r rlsa rlsp
  . Document o
  ⇒ FoldlRecord
      (ConstFolding DocumentInput)
      (NonEmptyArray FlowContentNode)
      rlsa
      isa
      (NonEmptyArray FlowContentNode)
  ⇒ FoldlRecord
      (ConstFolding DocumentInput)
      (Array NonEmptyString)
      rlsp
      isp
      (Array NonEmptyString)
  ⇒ Mapping GetValueSample osa o
  ⇒ Mapping GetValueDescription osa NonEmptyString
  ⇒ MapRecordWithIndex rlsa (ConstMapping GetValueSample) isa i
  ⇒ MapRecordWithIndex rlsa (ConstMapping ToValueSpec) isa isp
  ⇒ RowToList isa rlsa
  ⇒ RowToList isp rlsp
  ⇒ ComputationDocsSpec isa isp o osa r
  → NonEmptyString /\ Document
documentComputation
  { context, description, examples, input, output, properties } =
  docTitle /\ doc
  where
  doc ∷ Document
  doc =
    M.document $
      [ M.heading1
          $ ArrayNE.singleton
          $ M.text
          $ docTitle
      ]
        <> renderSchema
        <> renderContext
        <> renderProperties
        <> (renderExamples $ Map.toUnfoldableUnordered examplesByTitle)

  renderSchema ∷ Array FlowContentNode
  renderSchema =
    [ M.heading2 $ ArrayNE.singleton $ M.text $ StringNE.nes
        (Proxy ∷ Proxy "Schema")
    ] <> (ArrayNE.toArray $ documentSchema input output)

  renderContext ∷ Array FlowContentNode
  renderContext =
    [ M.heading2
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.nes
            (Proxy ∷ Proxy "Context")
    ] <> context

  renderExamples
    ∷ Array (NonEmptyString /\ (NonEmptyArray FlowContentNode))
    → Array FlowContentNode
  renderExamples = ArrayNE.fromFoldable >>>
    case _ of
      Just docsByIdx →
        [ M.heading2
            $ ArrayNE.singleton
            $ M.text
            $ StringNE.nes
                (Proxy ∷ Proxy "Examples")
        ]
          <> (renderExamplesIndex $ Tuple.fst <$> docsByIdx)
          <>
            ( Array.concat
                $ Array.fromFoldable
                $ Array.fromFoldable <<< Tuple.snd <$> docsByIdx
            )
      Nothing →
        []

  renderProperties ∷ Array FlowContentNode
  renderProperties = case ArrayNE.fromFoldable properties of
    Just props →
      [ M.heading2
          $ ArrayNE.singleton
          $ M.text
          $ StringNE.nes
              (Proxy ∷ Proxy "Properties")
      , M.unorderedList $ documentComputationProperty <$> props
      ]
    Nothing →
      []

  docTitle ∷ NonEmptyString
  docTitle = capitalizeTitle $ description output input

  renderExamplesIndex
    ∷ NonEmptyArray NonEmptyString → Array FlowContentNode
  renderExamplesIndex = Array.singleton
    <<< M.unorderedList
    <<< map \title →
      ArrayNE.singleton
        $ M.paragraph
        $ ArrayNE.singleton
        $ M.link title (M.urlOfHeading title)

  examplesByTitle ∷ Map NonEmptyString (NonEmptyArray FlowContentNode)
  examplesByTitle = Map.fromFoldable
    $ documentComputationExample <$> examples

  documentComputationExample
    ∷ ComputationExample isa osa
    → NonEmptyString /\ NonEmptyArray FlowContentNode
  documentComputationExample example =
    let
      exampleTitle = description
        output
        (hmap ToValueSpec example.input)
      exampleDoc =
        ( M.rule `ArrayNE.cons'`
            ( [ M.heading3
                  $ ArrayNE.singleton
                  $ M.text exampleTitle
              ]
            )
            <>
              ( ArrayNE.singleton
                  $ M.paragraph
                  $ ArrayNE.singleton
                  $ M.text example.description
              )
            <> documentInput example.input
        )
          <> documentOutput example.expectedOutput
    in
      exampleTitle /\ exampleDoc

  documentComputationProperty
    ∷ ComputationProperty isa o → NonEmptyArray FlowContentNode
  documentComputationProperty { description: propertyDesc } =
    ArrayNE.singleton
      $ M.paragraph
      $ ArrayNE.singleton
      $ M.text propertyDesc

capitalizeTitle ∷ NonEmptyString → NonEmptyString
capitalizeTitle = StringNE.uncons >>> \{ head, tail } →
  (StringNE.toUpper $ StringNE.singleton head)
    `StringNE.appendString` (maybe "" StringNE.toString tail)
