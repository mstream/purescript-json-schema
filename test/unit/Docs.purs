module Test.Docs where

import Prelude

import Data.Array ((!!))
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.FoldableWithIndex
  ( class FoldableWithIndex
  , foldMapWithIndex
  )
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process as Process
import Test.Spec.JsonSchema.Codec as Codec
import Test.Spec.JsonSchema.Codec.Parsing as Parsing
import Test.Spec.JsonSchema.Codec.Printing as Printing
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

main ∷ Effect Unit
main = launchAff_
  $ FS.writeTextFile UTF8 "docs/src/examples/README.generated.md"
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
  , input ∷ String
  , output ∷ String
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
  → String
printExamples examplesByCategory =
  "# Examples\n\n"
    <>
      ( printTableOfContents
          $ foldMapWithIndex
              (\category _ → [ category ])
              examplesByCategory
      )
    <> foldMapWithIndex printCategory examplesByCategory
  where
  printTableOfContents ∷ Array Category → String
  printTableOfContents = (_ <> "\n")
    <<< foldMap printTableOfContentsEntry

  printTableOfContentsEntry ∷ Category → String
  printTableOfContentsEntry category =
    "- ["
      <> renderCategory category
      <> "](#"
      <> (formatAnchor $ renderCategory category)
      <> ")\n"

  printCategory ∷ Category → Array PrintableExample → String
  printCategory category examples =
    "---\n"
      <> "## "
      <> renderCategory category
      <> "\n"
      <> foldMap printExample examples

  printExample ∷ PrintableExample → String
  printExample { description, input, output, title } =
    "### ⌘ "
      <> title
      <> "\n"
      <> description
      <> "\n#### Input\n"
      <> input
      <> "\n#### Output\n"
      <> output
      <> "\n\n"

  formatAnchor ∷ String → String
  formatAnchor = String.replaceAll (Pattern " ") (Replacement "-")
    <<< String.toLower
