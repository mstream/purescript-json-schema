module Test.Spec.JsonSchema.Diff (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import JsonSchema.Diff as Diff
import JsonSchema.Gen as SchemaGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), generativeTestCase)

spec ∷ TestSpec
spec = describe "Diff" do
  describe "calculate" do
    generativeTestCase Long "Identical schemata yield no differences."
      do
        schema ← SchemaGen.genSchema
        let
          actual = Diff.calculate schema schema
          expected = Nothing
        pure $ actual === expected
