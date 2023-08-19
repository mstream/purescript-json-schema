module Test.Spec.JsonSchema.Compatibility (examples, spec) where

import Prelude

import Data.Foldable (foldMap, traverse_)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import JsonSchema.Compatibility (Compatibility(..))
import JsonSchema.Compatibility as Compatibility
import JsonSchema.Diff (Difference)
import JsonSchema.Diff as Diff
import Test.Spec (describe)
import Test.Types (Example, TestSpec)
import Test.Utils (exampleTestCase)

type DiffExample = Example (Set Difference) Compatibility

renderInput ∷ Set Difference → String
renderInput differences =
  "##### JSON schema differences\n"
    <> "```json\n"
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

renderOutput ∷ Compatibility → String
renderOutput compatibility =
  "```\n"
    <> Compatibility.renderCompatibility compatibility
    <> "\n```"

transform ∷ Set Difference → Compatibility
transform = Compatibility.calculate

scenario
  ∷ String → String → Set Difference → Compatibility → DiffExample
scenario title description input expectedCompatibility =
  { description
  , expectedOutput: expectedCompatibility
  , input
  , renderInput
  , renderOutput
  , title
  , transform
  }

examples ∷ Array DiffExample
examples =
  [ scenario
      "Compatibility when there are no JSON schema differences"
      "When there is not JSON schema differences, schema change is fully compatible."
      Set.empty
      Full
  ]

spec ∷ TestSpec
spec = describe "Compatibility" do
  describe "calculate" do
    traverse_ exampleTestCase examples
