module Test.Main where

import Prelude

import Data.Array ((!!))
import Data.Foldable (class Foldable, sequence_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Exception (throw)
import Node.Process as Process
import Test.Spec.JsonSchema.Codec as Codec
import Test.Spec.JsonSchema.Codec.Parsing as Parsing
import Test.Spec.JsonSchema.Codec.Printing as Printing
import Test.Spec.JsonSchema.Diff as Diff
import Test.Spec.JsonSchema.Validation as Validation
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.Types (TestSpec)

main ∷ Effect Unit
main = do
  args ← Process.argv
  specs ← selectSpecs $ args !! 2
  launchAff_ $ runTestSpecs specs
  where
  selectSpecs ∷ Maybe String → Effect (Array TestSpec)
  selectSpecs = case _ of
    Nothing →
      pure allSpecs
    Just moduleName →
      case moduleName of
        "Codec" →
          pure [ Codec.spec ]
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

  allSpecs =
    [ Codec.spec
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
