module Test.Main where

import Prelude

import Data.Array ((!!))
import Data.Foldable (class Foldable, foldMap, foldl, sequence_)
import Data.FoldableWithIndex
  ( class FoldableWithIndex
  , foldMapWithIndex
  )
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
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
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.Types (Example, TestSpec)

main ∷ Effect Unit
main = do
  args ← Process.argv
  examples ← selectExamples $ args !! 2
  specs ← selectSpecs $ args !! 2

  launchAff_ do
    FS.writeTextFile UTF8 "Examples.generated.md"
      $ printExamples
      $ groupExamplesByCategory examples

    runTestSpecs specs
  where
  selectExamples ∷ Maybe String → Effect (Array PrintableExample)
  selectExamples = case _ of
    Nothing →
      pure allExamples
    Just moduleName →
      case moduleName of
        "Codec" →
          pure $ makePrintable "Codec" <$> Codec.examples
        "Compatibility" →
          pure $ makePrintable "Compatibility" <$>
            Compatibility.examples
        "Diff" →
          pure $ makePrintable "Diff" <$> Diff.examples
        "Parsing" →
          pure $ makePrintable "Parsing" <$> Parsing.examples
        "Printing" →
          pure $ makePrintable "Printing" <$> Printing.examples
        "Validation" →
          pure $ makePrintable "Validation" <$> Validation.examples
        _ →
          throw $ "Unknown module name \"" <> moduleName <> "\""

  selectSpecs ∷ Maybe String → Effect (Array TestSpec)
  selectSpecs = case _ of
    Nothing →
      pure allSpecs
    Just moduleName →
      case moduleName of
        "Codec" →
          pure [ Codec.spec ]
        "Compatibility" →
          pure [ Compatibility.spec ]
        "Diff" →
          pure [ Diff.spec ]
        "Parsing" →
          pure [ Parsing.spec ]
        "Printing" →
          pure [ Printing.spec ]
        "Validation" →
          pure [ Validation.spec ]
        _ →
          throw $ "Unknown module name \"" <> moduleName <> "\""

  allExamples ∷ Array PrintableExample
  allExamples =
    (makePrintable "Codec" <$> Codec.examples)
      <> (makePrintable "Compatibility" <$> Compatibility.examples)
      <> (makePrintable "Diff" <$> Diff.examples)
      <> (makePrintable "Parsing" <$> Parsing.examples)
      <> (makePrintable "Printing" <$> Printing.examples)
      <> (makePrintable "Validation" <$> Validation.examples)

  allSpecs ∷ Array TestSpec
  allSpecs =
    [ Codec.spec
    , Compatibility.spec
    , Diff.spec
    , Parsing.spec
    , Printing.spec
    , Validation.spec
    ]

runTestSpecs ∷ ∀ f. Foldable f ⇒ f TestSpec → Aff Unit
runTestSpecs specs = do
  resultsAff ← runSpecT
    defaultConfig
    [ consoleReporter ]
    (sequence_ specs)

  void resultsAff

type PrintableExample =
  { category ∷ String
  , description ∷ String
  , input ∷ String
  , output ∷ String
  , title ∷ String
  }

makePrintable ∷ ∀ i o. String → Example i o → PrintableExample
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
  → Map String (Array PrintableExample)
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
  . FoldableWithIndex String f
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
  printTableOfContents ∷ Array String → String
  printTableOfContents = (_ <> "\n")
    <<< foldMap printTableOfContentsEntry

  printTableOfContentsEntry ∷ String → String
  printTableOfContentsEntry category =
    "- [" <> category <> "](#" <> String.toLower category <> ")\n"

  printCategory ∷ String → Array PrintableExample → String
  printCategory category examples =
    "---\n"
      <> "## "
      <> category
      <> "\n"
      <> foldMap printExample examples

  printExample ∷ PrintableExample → String
  printExample { description, input, output, title } =
    "### ► "
      <> title
      <> "\n"
      <> description
      <> "\n#### Input\n"
      <> input
      <> "\n#### Output\n"
      <> output
      <> "\n\n"
