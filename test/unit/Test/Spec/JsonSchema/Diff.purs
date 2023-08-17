module Test.Spec.JsonSchema.Diff (examples, spec) where

import Prelude

import Data.Maybe (Maybe(..))
import JsonSchema.Diff as Diff
import JsonSchema.Gen as SchemaGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (Example, TestLength(..), TestSpec)
import Test.Utils (generativeTestCase)

type DiffExample = Example Unit Unit

examples ∷ Array DiffExample
examples = []

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
