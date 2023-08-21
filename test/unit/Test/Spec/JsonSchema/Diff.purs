module Test.Spec.JsonSchema.Diff (examples, spec) where

import Prelude

import Data.Argonaut.Core as A
import Data.Foldable (foldMap, traverse_)
import Data.List (List(..))
import Data.Markdown (CodeBlockType(..), Document)
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.Codec.Printing as Printing
import JsonSchema.Diff (Difference, DifferenceType(..))
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

renderInput ∷ DiffExampleInput → Document
renderInput { nextSchema, previousSchema } =
  [ M.heading5 "Previous JSON schema"
  , M.codeBlock Json
      $ (A.stringify <<< Printing.printSchema) previousSchema
  , M.heading5 "Next JSON schema"
  , M.codeBlock Json
      $ (A.stringify <<< Printing.printSchema) nextSchema
  ]

renderOutput ∷ Set Difference → Document
renderOutput differences =
  [ M.codeBlock' $ String.joinWith "\n" renderDifferences ]
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
  , scenario
      "Changing expected JSON value type from null to boolean"
      "Any change in expected JSON value type should be accounted as a difference."
      { nextSchema: ObjectSchema
          $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.singleton JsonBoolean }
      , previousSchema: ObjectSchema
          $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.singleton JsonNull }
      }
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonNull)
              (Just $ Set.singleton JsonBoolean)
          , path: Nil
          }
      )
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
