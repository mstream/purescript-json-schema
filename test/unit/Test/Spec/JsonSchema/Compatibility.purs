module Test.Spec.JsonSchema.Compatibility (examples, spec) where

import Prelude

import Data.Foldable (foldMap, traverse_)
import Data.List (List(..))
import Data.Markdown (Document)
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import JsonSchema (JsonValueType(..))
import JsonSchema.Compatibility (Compatibility(..))
import JsonSchema.Compatibility as Compatibility
import JsonSchema.Diff (Difference, DifferenceType(..))
import JsonSchema.Diff as Diff
import Test.Spec (describe)
import Test.Types (Example, TestSpec)
import Test.Utils (exampleTestCase)

type DiffExample = Example (Set Difference) Compatibility

renderInput ∷ Set Difference → Document
renderInput differences =
  [ M.heading5 "JSON schema differences"
  , M.codeBlock' $ String.joinWith "\n" renderDifferences
  ]
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

renderOutput ∷ Compatibility → Document
renderOutput compatibility =
  [ M.codeBlock' $ Compatibility.renderCompatibility compatibility ]

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
      "No JSON schema differences"
      "When there is not JSON schema differences, schema change is fully compatible."
      Set.empty
      Full
  , scenario
      "Expected JSON value type changes from null to boolean"
      "Because no boolean value can satisfy null JSON type constraint, and vice versa, such a change is incompatible."
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonNull)
              (Just $ Set.singleton JsonBoolean)
          , path: Nil
          }
      )
      None
  , scenario
      "Expected JSON value type changes from integer to number"
      "Because every integer is a number, but not vice versa, such a change is backward compatible."
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonInteger)
              (Just $ Set.singleton JsonNumber)
          , path: Nil
          }
      )
      Backward
  , scenario
      "Expected JSON value type changes from number to integer"
      "Because every integer is a number, but not vice versa, such a change is forward compatible."
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonNumber)
              (Just $ Set.singleton JsonInteger)
          , path: Nil
          }
      )
      Forward
  , scenario
      "Expected JSON value types is extended"
      "Because more value types than before are accepted, this change is backward compatible."
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonNull)
              (Just $ Set.fromFoldable [ JsonBoolean, JsonNull ])
          , path: Nil
          }
      )
      Backward
  , scenario
      "Expected JSON value types is reduced"
      "Because less value types than before are accepted, this change is forward compatible."
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.fromFoldable [ JsonBoolean, JsonNull ])
              (Just $ Set.singleton JsonNull)
          , path: Nil
          }
      )
      Forward
  , scenario
      "Expected JSON value types including number is extended by integer"
      "Because every integer is a number, such a change is fully compatible."
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonNumber)
              (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
          , path: Nil
          }
      )
      Full
  , scenario
      "Expected JSON value types including integer is extended by number"
      "Because not every integer is a number, such a change is backward compatible."
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonInteger)
              (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
          , path: Nil
          }
      )
      Backward
  , scenario
      "Expected JSON value types including integer and number is reduced by integer"
      "Because every integer is a number, such a change is fully compatible."
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
              (Just $ Set.singleton JsonNumber)
          , path: Nil
          }
      )
      Full
  , scenario
      "Expected JSON value types including integer and number is reuced by number"
      "Because not every integer is a number, such a change is forward compatible."
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
              (Just $ Set.singleton JsonInteger)
          , path: Nil
          }
      )
      Forward
  , scenario
      "the new value of multipleOf is divisible by the old one"
      "Because every multiple the new value is also a multiple of the old value, such a change is backward compatible"
      ( Set.singleton
          { differenceType: MultipleOfChange (Just 2.0) (Just 4.0)
          , path: Nil
          }
      )
      Backward
  , scenario
      "the old value of multipleOf is divisible by the new one"
      "Because every multiple the old value is also a multiple of the new value, such a change is forward compatible"
      ( Set.singleton
          { differenceType: MultipleOfChange (Just 4.0) (Just 2.0)
          , path: Nil
          }
      )
      Forward
  , scenario
      "old and new value of multipleOf are not each other's factors"
      "In this situation, there are potentially some numbers that are not divisible by neither of multipleOf values. Therefore, such a change is incompatible."
      ( Set.singleton
          { differenceType: MultipleOfChange (Just 2.0) (Just 5.0)
          , path: Nil
          }
      )
      None
  ]

spec ∷ TestSpec
spec = describe "Compatibility" do
  describe "calculate" do
    traverse_ exampleTestCase examples
