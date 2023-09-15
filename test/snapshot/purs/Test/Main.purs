module Test.Snapshot.Main (main) where

import Prelude

import Data.Foldable (class Foldable, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Snapshot.Spec.Docs as Docs
import Test.Snapshot.TestSpec (TestSpec)
import Test.Snapshot.Utils (testSnapshot)
import Test.Spec as Spec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

main ∷ Effect Unit
main = do
  launchAff_ $ runTestSpecs specs
  where
  specs ∷ Array TestSpec
  specs =
    [ Spec.describe "Docs" $ testSnapshot Docs.spec ]

runTestSpecs ∷ ∀ f. Foldable f ⇒ f TestSpec → Aff Unit
runTestSpecs specs = do
  resultsAff ← runSpecT
    defaultConfig
    [ consoleReporter ]
    (sequence_ specs)

  void resultsAff
