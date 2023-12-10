module Test.Unit.Main (main) where

import Prelude

import Data.Foldable (class Foldable, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Computation.Utils (testComputation)
import Test.Spec as Spec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.TestSpec (TestSpec)
import Test.Unit.Spec.Data.Markdown as Markdown
import Test.Unit.Spec.JsonSchema as Schema
import Test.Unit.Spec.JsonSchema.Codec.Parsing as Parsing
import Test.Unit.Spec.JsonSchema.Compatibility as Compatibility
import Test.Unit.Spec.JsonSchema.Difference as Difference
import Test.Unit.Spec.JsonSchema.Validation as Validation

main ∷ Effect Unit
main = do
  launchAff_ $ runTestSpecs specs
  where
  specs ∷ Array TestSpec
  specs =
    [ Spec.describe "JsonSchema.Codec.Parsing"
        $ testComputation Parsing.spec
    , Spec.describe "JsonSchema (printing)"
        $ testComputation Schema.spec
    , Spec.describe "JsonSchema.Compatibility"
        $ testComputation Compatibility.spec
    , Spec.describe "JsonSchema.Difference"
        $ testComputation Difference.spec
    , Spec.describe "JsonSchema.Validation"
        $ testComputation Validation.spec
    , Spec.describe "Data.Markdown" Markdown.spec
    ]

runTestSpecs ∷ ∀ f. Foldable f ⇒ f TestSpec → Aff Unit
runTestSpecs specs = do
  resultsAff ← runSpecT
    defaultConfig
    [ consoleReporter ]
    (sequence_ specs)

  void resultsAff
