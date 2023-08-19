module Test.Spec.JsonSchema.Diff (examples, spec) where

import Prelude

import Data.Argonaut.Core as A
import Data.Foldable (foldMap, traverse_)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import JsonSchema (JsonSchema(..))
import JsonSchema.Codec.Printing as Printing
import JsonSchema.Diff (Difference(..))
import JsonSchema.Diff as Diff
import JsonSchema.Gen as SchemaGen
import Test.QuickCheck ((===))
import Test.Spec (describe)
import Test.Types (Example, TestLength(..), TestSpec)
import Test.Utils (exampleTestCase, generativeTestCase)

type DiffExampleInput =
  { nextSchema ∷ JsonSchema
  , previousSchema ∷ JsonSchema
  }

type DiffExample = Example DiffExampleInput (Set Difference)

renderInput ∷ DiffExampleInput → String
renderInput { nextSchema, previousSchema } =
  "##### Previous JSON schema\n"
    <> "```json\n"
    <> (A.stringify <<< Printing.printSchema) previousSchema
    <> "\n```\n"
    <> "##### Next JSON schema\n"
    <> "```json\n"
    <> (A.stringify <<< Printing.printSchema) nextSchema
    <> "\n```"

renderOutput ∷ Set Difference → String
renderOutput differences =
  "```\n"
    <> String.joinWith "\n" renderDifferences
    <> "\n```"
  where
  renderDifferences ∷ Array String
  renderDifferences =
    if Set.isEmpty differences then [ "no differences" ]
    else foldMap
      ( \difference →
          [ "-" ]
            <> (("  " <> _) <$> Diff.renderDifference difference)
      )
      differences

transform ∷ DiffExampleInput → Set Difference
transform { nextSchema, previousSchema } =
  Diff.calculate previousSchema nextSchema

scenario
  ∷ String → String → DiffExampleInput → Set Difference → DiffExample
scenario title description input expectedDifferences =
  { description
  , expectedOutput: expectedDifferences
  , input
  , renderInput
  , renderOutput
  , title
  , transform
  }

examples ∷ Array DiffExample
examples =
  [ scenario
      "Comparing identical schemata"
      "When two identical schemata are compared, no difference should be found."
      { nextSchema: BooleanSchema false
      , previousSchema: BooleanSchema false
      }
      Set.empty
  ]

spec ∷ TestSpec
spec = describe "Diff" do
  describe "calculate" do
    traverse_ exampleTestCase examples

    generativeTestCase Long "Identical schemata yield no differences."
      do
        schema ← SchemaGen.genSchema
        let
          actual = Diff.calculate schema schema
          expected = Set.empty
        pure $ actual === expected
