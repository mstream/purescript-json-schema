module Test.Snapshot.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Snapshot.Spec.Docs as Docs
import Test.Snapshot.Utils (testSnapshot)
import Test.Spec as Spec
import Test.TestSpec (TestSpec)
import Test.TestSpec as TestSpec

main ∷ Effect Unit
main = do
  launchAff_ $ TestSpec.runAll specs
  where
  specs ∷ Array TestSpec
  specs = [ Spec.describe "Docs" $ testSnapshot Docs.spec ]
