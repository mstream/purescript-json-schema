module Test.Spec.JsonSchema.Compatibility (computation, doc, spec) where

import Prelude

import Data.Foldable (foldMap, traverse_)
import Data.List (List(..))
import Data.Markdown (Document)
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Docs.Types (Doc)
import JsonSchema (JsonValueType(..))
import JsonSchema.Compatibility (Compatibility(..))
import JsonSchema.Compatibility as Compatibility
import JsonSchema.Diff (Difference, DifferenceType(..))
import JsonSchema.Diff as Diff
import Test.Spec (describe)
import Test.Types
  ( Computation
  , Example
  , ExpectedOutput
  , Input
  , Property
  , TestSpec
  )
import Test.Utils (exampleTestCase, exampleTitle, propertyTitle)

type CompatibilityInput = Set Difference
type CompatibilityOutput = Compatibility

type CompatibilityExample = Example
  CompatibilityInput
  CompatibilityOutput

type CompatibilityProperty = Property
  CompatibilityInput
  CompatibilityOutput

doc ∷ Doc
doc =
  { computationDescription: computation.description
  , examples: Set.fromFoldable $ examples <#> \example →
      { description: example.description
      , output: example.renderOutput example.expectedOutput
      , input: example.renderInput example.input
      , title: exampleTitle example
      }
  , properties: Set.fromFoldable $ properties <#> \property →
      { title: propertyTitle property }
  }

renderInput ∷ Input (Set Difference) → Document
renderInput { value: differences } =
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

renderOutput ∷ ExpectedOutput Compatibility → Document
renderOutput { value: compatibility } =
  [ M.codeBlock' $ Compatibility.renderCompatibility compatibility ]

computation ∷ Computation CompatibilityInput CompatibilityOutput
computation =
  { description: "calculating compatibility based on"
  , execute: Compatibility.calculate
  }

scenario
  ∷ String
  → Input (Set Difference)
  → Compatibility
  → CompatibilityExample
scenario description input expectedCompatibility =
  { computation
  , description
  , expectedOutput:
      { description: describeCompatibility expectedCompatibility
      , value: expectedCompatibility
      }
  , input
  , renderInput
  , renderOutput
  }

describeCompatibility ∷ Compatibility → String
describeCompatibility = case _ of
  Backward →
    "backward compatible"
  Forward →
    "forward compatible"
  Full →
    "fully compatible"
  None →
    "incompatible"

properties ∷ Array CompatibilityProperty
properties = []

examples ∷ Array CompatibilityExample
examples =
  [ scenario
      "When there is no JSON schema differences, schema change is fully compatible."
      { description: "no differences", value: Set.empty }
      Full
  , scenario
      "Because no boolean value can satisfy null JSON type constraint, and vice versa, such a change is incompatible."
      { description:
          "expected JSON value type changing from null to boolean"
      , value: Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonNull)
              (Just $ Set.singleton JsonBoolean)
          , path: Nil
          }
      }
      None
  , scenario
      "Because every integer is a number, but not vice versa, such a change is backward compatible."
      { description:
          "expected JSON value type changing from integer to number"
      , value: Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonInteger)
              (Just $ Set.singleton JsonNumber)
          , path: Nil
          }
      }
      Backward
  , scenario
      "Because every integer is a number, but not vice versa, such a change is forward compatible."
      { description:
          "expected JSON value type changing from number to integer"
      , value: Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonNumber)
              (Just $ Set.singleton JsonInteger)
          , path: Nil
          }
      }
      Forward
  , scenario
      "Because more value types than before are accepted, this change is backward compatible."
      { description:
          "expected JSON value types being extended from just null to null and boolean"
      , value: Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonNull)
              (Just $ Set.fromFoldable [ JsonBoolean, JsonNull ])
          , path: Nil
          }
      }
      Backward
  , scenario
      "Because less value types than before are accepted, this change is forward compatible."
      { description:
          "expected JSON value types being reduced from null and boolean to just null"
      , value: Set.singleton
          { differenceType: TypeChange
              (Just $ Set.fromFoldable [ JsonBoolean, JsonNull ])
              (Just $ Set.singleton JsonNull)
          , path: Nil
          }
      }
      Forward
  , scenario
      "Because every integer is a number, such a change is fully compatible."
      { description:
          "expected JSON value types including number being extended by integer"
      , value: Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonNumber)
              (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
          , path: Nil
          }
      }
      Full
  , scenario
      "Because not every integer is a number, such a change is backward compatible."
      { description:
          "expected JSON value types including integer being extended by number"
      , value: Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonInteger)
              (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
          , path: Nil
          }
      }
      Backward
  , scenario
      "Because every integer is a number, such a change is fully compatible."
      { description:
          "expected JSON value types including integer and number being reduced by integer"
      , value: Set.singleton
          { differenceType: TypeChange
              (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
              (Just $ Set.singleton JsonNumber)
          , path: Nil
          }
      }
      Full
  , scenario
      "Because not every integer is a number, such a change is forward compatible."
      { description:
          "expected JSON value types including integer and number being reuced by number"
      , value: Set.singleton
          { differenceType: TypeChange
              (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
              (Just $ Set.singleton JsonInteger)
          , path: Nil
          }
      }
      Forward
  , scenario
      "Because every multiple the new value is also a multiple of the old value, such a change is backward compatible"
      { description:
          "the new value of multipleOf being divisible by the old one"
      , value: Set.singleton
          { differenceType: MultipleOfChange (Just 2.0) (Just 4.0)
          , path: Nil
          }
      }
      Backward
  , scenario
      "Because every multiple the old value is also a multiple of the new value, such a change is forward compatible"
      { description:
          "the old value of multipleOf being divisible by the new one"
      , value: Set.singleton
          { differenceType: MultipleOfChange (Just 4.0) (Just 2.0)
          , path: Nil
          }
      }
      Forward
  , scenario
      "In this situation, there are potentially some numbers that are not divisible by neither of multipleOf values. Therefore, such a change is incompatible."
      { description:
          "old and new value of multipleOf being not each other's factors"
      , value:
          Set.singleton
            { differenceType: MultipleOfChange (Just 2.0) (Just 5.0)
            , path: Nil
            }
      }
      None
  , scenario
      "In this situation, all numbers from the new, longer range fall into the old, shorter range. Therefore, such a change is backward compatible."
      { description: "the range of allowed number values being extended"
      , value:
          Set.fromFoldable
            [ { differenceType: MaximumChange (Just 15.0) (Just 20.0)
              , path: Nil
              }
            , { differenceType: MinimumChange (Just 10.0) (Just 5.0)
              , path: Nil
              }
            ]
      }
      Backward
  , scenario
      "In this situation, all numbers from the new, shorted range fall into the old, longer range. Therefore, such a change is forward compatible."
      { description: "the range of allowed number values being reduced"
      , value:
          Set.fromFoldable
            [ { differenceType: MaximumChange (Just 20.0) (Just 15.0)
              , path: Nil
              }
            , { differenceType: MinimumChange (Just 5.0) (Just 10.0)
              , path: Nil
              }
            ]
      }
      Forward
  , scenario
      "In this situation, there are some numbers which do not fall into neither old nor new range. Therefore, such a change is incompatible."
      { description:
          "the range of allowed number values being extended and reduced at the same time"
      , value: Set.fromFoldable
          [ { differenceType: MaximumChange (Just 15.0) (Just 20.0)
            , path: Nil
            }
          , { differenceType: MinimumChange (Just 5.0) (Just 10.0)
            , path: Nil
            }
          ]
      }
      None
  ]

spec ∷ TestSpec
spec = describe "Compatibility" do
  describe "calculate" do
    traverse_ exampleTestCase examples
