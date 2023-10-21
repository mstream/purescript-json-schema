module Test.Unit.Spec.JsonSchema.Compatibility (spec) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.List (List(..))
import Data.Markdown (FlowContentNode)
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.Mermaid.FlowChart (FlowChartDef(..), Orientation(..))
import Data.Mermaid.FlowChart as FlowChart
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as SetNE
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import JsonSchema (JsonValueType(..))
import JsonSchema.Compatibility
  ( BackwardIncompatibility(..)
  , BackwardIncompatibilityType(..)
  , Compatibility(..)
  , ForwardIncompatibility(..)
  , ForwardIncompatibilityType(..)
  , NumberRangeChange(..)
  )
import JsonSchema.Compatibility as Compatibility
import JsonSchema.Difference (Difference(..), DifferenceType(..))
import JsonSchema.Range (Boundary(..))
import Test.Unit.Computation
  ( ComputationContext
  , ComputationExample
  , ComputationProperty
  , ComputationSpec
  , ValueSample(..)
  , ValueSpec(..)
  )
import Type.Proxy (Proxy(..))

type Input = (differences ∷ Set Difference)
type InputSpec = (differences ∷ ValueSpec (Set Difference))
type InputSample = (differences ∷ ValueSample (Set Difference))
type Output = Compatibility
type OutputSample = ValueSample Output

type Spec = ComputationSpec
  InputSample
  InputSpec
  Output
  OutputSample

type Example = ComputationExample InputSample OutputSample
type Property = ComputationProperty InputSample Output

spec ∷ Spec
spec =
  { context
  , description: \{ differences: ValueSpec differencesDesc } →
      StringNE.nes (Proxy ∷ Proxy "assessing compatibility based on ")
        <> differencesDesc
  , examples
  , execute: \{ differences: ValueSample differences } →
      Compatibility.calculate differences.sample
  , input:
      { differences: ValueSpec $ StringNE.nes
          (Proxy ∷ Proxy "schemata differences")
      }
  , output: ValueSpec $ StringNE.nes
      (Proxy ∷ Proxy "a type of compatibility")
  , properties
  }

context ∷ ComputationContext
context =
  [ compatibilityTypeParagraph
      ( StringNE.nes
          (Proxy ∷ Proxy "Backward")
      )
      ( StringNE.nes
          ( Proxy
              ∷ Proxy
                  "an ability of a system to understand input intended for previous versions of itself"
          )
      )
  , compatibilityTypeParagraph
      ( StringNE.nes
          (Proxy ∷ Proxy "Forward")
      )
      ( StringNE.nes
          ( Proxy
              ∷ Proxy
                  "an ability of a system to understand input intended for future versions of itself"
          )
      )
  , compatibilityTypeParagraph
      ( StringNE.nes
          (Proxy ∷ Proxy "Full")
      )
      ( StringNE.nes
          (Proxy ∷ Proxy "backward and forward compatibility combined")
      )
  , compatibilityTypeParagraph
      ( StringNE.nes
          (Proxy ∷ Proxy "No")
      )
      ( StringNE.nes
          (Proxy ∷ Proxy "neither level of compatibility")
      )
  , M.paragraph $
      ( M.text $
          StringNE.nes
            ( Proxy
                ∷ Proxy
                    "Maintaining backward and forward compatibility is important for minimizing disruption"
            )
      )
        `ArrayNE.cons'`
          [ M.lineBreak
          , M.text
              $ StringNE.nes
                  ( Proxy
                      ∷ Proxy
                          "and ensuring smooth transitions when updating JSON schemas."
                  )
          ]
  , M.renderMermaid $ FlowChartDef LeftToRight
      [ FlowChart.subGraph "data writers"
          [ FlowChart.capsule "current_writer" "writer"
          , FlowChart.capsule "next_writer"
              "writer<sub>+1</sub>"
          ]
      , FlowChart.subGraph "data readers"
          [ FlowChart.capsule "current_reader" "reader"
          , FlowChart.capsule "next_reader"
              "reader<sub>+1</sub>"
          ]
      , FlowChart.normalArrow
          "current_writer"
          "current_reader"
      , FlowChart.normalArrowWithAnnotation
          "backward compatibility"
          "current_writer"
          "next_reader"
      , FlowChart.normalArrowWithAnnotation
          "forward compatibility"
          "next_writer"
          "current_reader"
      ]
  , M.renderMermaid $ FlowChartDef LeftToRight
      [ FlowChart.subGraph "data writers"
          [ FlowChart.capsule "current_writer" "writer"
          , FlowChart.capsule "next_writer"
              "writer<sub>+1</sub>"
          ]
      , FlowChart.subGraph "data readers"
          [ FlowChart.capsule "current_reader" "reader"
          , FlowChart.capsule "next_reader"
              "reader<sub>+1</sub>"
          ]
      , FlowChart.normalArrow
          "current_writer"
          "current_reader"
      , FlowChart.normalArrowWithAnnotation
          "backward compatibility"
          "current_writer"
          "next_reader"
      , FlowChart.normalArrowWithAnnotation
          "forward compatibility"
          "next_writer"
          "current_reader"
      ]
  ]
  where
  compatibilityTypeParagraph
    ∷ NonEmptyString → NonEmptyString → FlowContentNode
  compatibilityTypeParagraph compatibilityType description =
    M.paragraph $
      M.emphasis
        ( ArrayNE.singleton $ M.text
            $ compatibilityType <> StringNE.nes
                (Proxy ∷ Proxy " compatibility")
        )
        `ArrayNE.cons'`
          [ M.text $ StringNE.nes (Proxy ∷ Proxy " - ")
          , M.text description
          ]

properties ∷ Array Property
properties = []

examples ∷ Array Example
examples =
  [ fullCompatibilityExample
      "identical schemata cannot be incompatible with each other"
      { differences: ValueSample
          { description: StringNE.nes (Proxy ∷ Proxy "no differences")
          , sample: Set.empty
          }
      }
  , fullCompatibilityExample
      "every integer is also a number so this kind of difference does not have any implact"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "extending set of accepted value types from numbers to numbers and integers"
              )
          , sample: Set.singleton $ Difference
              { differenceType: TypeChange
                  (Just $ Set.singleton JsonNumber)
                  (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
              , path: Nil
              }
          }
      }
  , backwardCompatibilityExample
      "Because not every integer is a number, such a change is backward compatible."
      { differences: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "extension of accepted types by a number")
          , sample: Set.singleton $ Difference
              { differenceType: TypeChange
                  (Just $ Set.singleton JsonInteger)
                  (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
              , path: Nil
              }
          }
      }
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType: SetOfAllowedTypesExtended
              $ SetNE.singleton JsonNumber
          , path: Nil
          }
      )
  , forwardCompatibilityExample
      "Because not every integer is a number, such a change is forward compatible."
      { differences: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "reduction of accepted types by a number")
          , sample: Set.singleton $ Difference
              { differenceType: TypeChange
                  (Just $ Set.fromFoldable [ JsonInteger, JsonNumber ])
                  (Just $ Set.singleton JsonInteger)
              , path: Nil
              }
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType: SetOfAllowedTypesReduced
              $ SetNE.singleton JsonNumber
          , path: Nil
          }
      )
  , noCompatibilityExample
      "Because no boolean value can satisfy null JSON type constraint, and vice versa, such a change is incompatible."
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy "an accepted type change from null to boolean"
              )
          , sample: Set.singleton $ Difference
              { differenceType: TypeChange
                  (Just $ Set.singleton JsonNull)
                  (Just $ Set.singleton JsonBoolean)
              , path: Nil
              }
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType: SetOfAllowedTypesReduced $
              SetNE.singleton JsonNull
          , path: Nil
          }
      )
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType: SetOfAllowedTypesExtended $
              SetNE.singleton JsonBoolean
          , path: Nil
          }
      )
  , backwardCompatibilityExample
      "Because every integer is a number, but not vice versa, such a change is backward compatible."
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "change of accepted type from integer to number"
              )
          , sample: Set.singleton $ Difference
              { differenceType: TypeChange
                  (Just $ Set.singleton JsonInteger)
                  (Just $ Set.singleton JsonNumber)
              , path: Nil
              }
          }
      }
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType: SetOfAllowedTypesExtended
              $ SetNE.singleton JsonNumber
          , path: Nil
          }
      )
  , forwardCompatibilityExample
      "Because every integer is a number, but not vice versa, such a change is forward compatible."
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "change of accepted type from number to integer"
              )
          , sample: Set.singleton $ Difference
              { differenceType: TypeChange
                  (Just $ Set.singleton JsonNumber)
                  (Just $ Set.singleton JsonInteger)
              , path: Nil
              }
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType: SetOfAllowedTypesReduced
              $ SetNE.singleton JsonNumber
          , path: Nil
          }
      )
  , backwardCompatibilityExample
      "Because more value types than before are accepted, this change is backward compatible."
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "extension of accepted types by an additional type"
              )
          , sample: Set.singleton $ Difference
              { differenceType: TypeChange
                  (Just $ Set.singleton JsonNull)
                  (Just $ Set.fromFoldable [ JsonNull, JsonBoolean ])
              , path: Nil
              }
          }
      }
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType: SetOfAllowedTypesExtended
              $ SetNE.singleton JsonBoolean
          , path: Nil
          }
      )
  , forwardCompatibilityExample
      "Because less value types than before are accepted, this change is backward compatible."
      { differences: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "reduction of accepted types")
          , sample: Set.singleton $ Difference
              { differenceType: TypeChange
                  (Just $ Set.fromFoldable [ JsonNull, JsonBoolean ])
                  (Just $ Set.singleton JsonNull)
              , path: Nil
              }
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType: SetOfAllowedTypesReduced
              $ SetNE.singleton JsonBoolean
          , path: Nil
          }
      )
  , backwardCompatibilityExample
      "Because every multiple the new value is also a multiple of the old value, such a change is backward compatible"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "the new value of multipleOf being divisible by the old one"
              )
          , sample: Set.singleton $ Difference
              { differenceType: MultipleOfChange (Just 2.0) (Just 4.0)
              , path: Nil
              }
          }
      }
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType: NewMultipleIsNotFactorOfOldMultiple
              { new: 4.0, old: 2.0 }
          , path: Nil
          }
      )
  , forwardCompatibilityExample
      "Because every multiple the old value is also a multiple of the new value, such a change is forward compatible"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "the old value of multipleOf being divisible by the new one"
              )
          , sample: Set.singleton $ Difference
              { differenceType: MultipleOfChange (Just 4.0) (Just 2.0)
              , path: Nil
              }
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType: OldMultipleIsNotFactorOfNewMultiple
              { new: 2.0, old: 4.0 }
          , path: Nil
          }
      )
  , noCompatibilityExample
      "there are potentially some numbers that are not divisible by neither of multipleOf values"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "old and new value of multipleOf being not each other's factors"
              )
          , sample: Set.singleton $ Difference
              { differenceType: MultipleOfChange (Just 2.0) (Just 5.0)
              , path: Nil
              }
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType: OldMultipleIsNotFactorOfNewMultiple
              { new: 5.0, old: 2.0 }
          , path: Nil
          }
      )
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType: NewMultipleIsNotFactorOfOldMultiple
              { new: 5.0, old: 2.0 }
          , path: Nil
          }
      )
  , forwardCompatibilityExample
      "all numbers from the new range fall into the old, unconstrained one"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "the old value of multipleOf being divisible by the new one"
              )
          , sample:
              Set.fromFoldable
                [ Difference
                    { differenceType: MaximumChange Nothing (Just 20.0)
                    , path: Nil
                    }
                , Difference
                    { differenceType: MinimumChange Nothing (Just 5.0)
                    , path: Nil
                    }
                ]
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType: RangeOfAllowedNumbersReduced
              $ LowerAndUpper
                  { from: Open bottom, to: Open 5.0 }
                  { from: Open 20.0, to: Open top }
          , path: Nil
          }
      )
  , backwardCompatibilityExample
      "all numbers from the old range fall into the new, unconstrained one"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "the range gets unconstrained"
              )
          , sample:
              Set.fromFoldable
                [ Difference
                    { differenceType: MaximumChange (Just 20.0) Nothing
                    , path: Nil
                    }
                , Difference
                    { differenceType: MinimumChange (Just 5.0) Nothing
                    , path: Nil
                    }
                ]
          }
      }
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType: RangeOfAllowedNumbersExtended
              $ LowerAndUpper
                  { from: Open bottom, to: Closed 5.0 }
                  { from: Open 20.0, to: Open top }
          , path: Nil
          }
      )
  , backwardCompatibilityExample
      "all numbers from the new, longer range fall into the old, shorter range"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "the range of allowed number values being extended"
              )
          , sample:
              Set.fromFoldable
                [ Difference
                    { differenceType: MaximumChange
                        (Just 15.0)
                        (Just 20.0)
                    , path: Nil
                    }
                , Difference
                    { differenceType: MinimumChange
                        (Just 10.0)
                        (Just 5.0)
                    , path: Nil
                    }
                ]
          }
      }
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType:
              RangeOfAllowedNumbersExtended
                $ LowerAndUpper
                    { from: Closed 5.0, to: Open 10.0 }
                    { from: Open 15.0, to: Closed 20.0 }
          , path: Nil
          }
      )
  , backwardCompatibilityExample
      "all numbers from the new, longer range fall into the old, shorter range"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "the range of allowed values being shifted"
              )
          , sample:
              Set.fromFoldable
                [ Difference
                    { differenceType: ExclusiveMaximumChange
                        (Just 15.0)
                        (Just 20.0)
                    , path: Nil
                    }
                , Difference
                    { differenceType: ExclusiveMinimumChange
                        (Just 10.0)
                        (Just 5.0)
                    , path: Nil
                    }
                ]
          }
      }
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType:
              RangeOfAllowedNumbersExtended
                $ LowerAndUpper
                    { from: Open 5.0, to: Closed 10.0 }
                    { from: Closed 15.0, to: Open 20.0 }
          , path: Nil
          }
      )
  , forwardCompatibilityExample
      "all numbers from the new, shorted range fall into the old, longer range"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "the range of allowed values being reduced"
              )
          , sample:
              Set.fromFoldable
                [ Difference
                    { differenceType: MaximumChange
                        (Just 20.0)
                        (Just 15.0)
                    , path: Nil
                    }
                , Difference
                    { differenceType: MinimumChange
                        (Just 5.0)
                        (Just 10.0)
                    , path: Nil
                    }
                ]
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType:
              RangeOfAllowedNumbersReduced
                $ LowerAndUpper
                    { from: Closed 5.0, to: Open 10.0 }
                    { from: Open 15.0, to: Closed 20.0 }
          , path: Nil
          }
      )
  , forwardCompatibilityExample
      "all numbers from the new, shorted range fall into the old, longer range"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "the range of allowed values being reduced using the exclusive version of constraints"
              )
          , sample:
              Set.fromFoldable
                [ Difference
                    { differenceType: ExclusiveMaximumChange
                        (Just 20.0)
                        (Just 15.0)
                    , path: Nil
                    }
                , Difference
                    { differenceType: ExclusiveMinimumChange
                        (Just 5.0)
                        (Just 10.0)
                    , path: Nil
                    }
                ]
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType:
              RangeOfAllowedNumbersReduced
                $ LowerAndUpper
                    { from: Open 5.0, to: Closed 10.0 }
                    { from: Closed 15.0, to: Open 20.0 }
          , path: Nil
          }
      )
  , noCompatibilityExample
      "there are some numbers which do not fall into neither old nor new range"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "the range of allowed number values being extended and reduced at the same time"
              )
          , sample:
              Set.fromFoldable
                [ Difference
                    { differenceType: MaximumChange
                        (Just 15.0)
                        (Just 20.0)
                    , path: Nil
                    }
                , Difference
                    { differenceType: MinimumChange
                        (Just 5.0)
                        (Just 10.0)
                    , path: Nil
                    }
                ]
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType:
              RangeOfAllowedNumbersReduced
                $ Lower { from: Closed 5.0, to: Open 10.0 }
          , path: Nil
          }
      )
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType:
              RangeOfAllowedNumbersExtended
                $ Upper { from: Open 15.0, to: Closed 20.0 }
          , path: Nil
          }
      )
  , noCompatibilityExample
      "there are some numbers which do not fall into neither old nor new range"
      { differences: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "the range of allowed number values being extended and reduced at the same time using exclusive versions of constraints"
              )
          , sample:
              Set.fromFoldable
                [ Difference
                    { differenceType: ExclusiveMaximumChange
                        (Just 15.0)
                        (Just 20.0)
                    , path: Nil
                    }
                , Difference
                    { differenceType: ExclusiveMinimumChange
                        (Just 5.0)
                        (Just 10.0)
                    , path: Nil
                    }
                ]
          }
      }
      ( SetNE.singleton $ BackwardIncompatibility
          { incompatibilityType:
              RangeOfAllowedNumbersReduced
                $ Lower { from: Open 5.0, to: Closed 10.0 }
          , path: Nil
          }
      )
      ( SetNE.singleton $ ForwardIncompatibility
          { incompatibilityType:
              RangeOfAllowedNumbersExtended
                $ Upper { from: Closed 15.0, to: Open 20.0 }
          , path: Nil
          }
      )
  ]

fullCompatibilityExample ∷ String → { | InputSample } → Example
fullCompatibilityExample description input =
  { description:
      StringNE.nes (Proxy ∷ Proxy "In this situation, ")
        `StringNE.appendString` description
        <> StringNE.nes
          ( Proxy
              ∷ Proxy
                  ". Therefore, such a change is fully compatible."
          )

  , expectedOutput: ValueSample
      { description:
          StringNE.nes (Proxy ∷ Proxy "full compatibility")
      , sample: Full
      }
  , input
  }

backwardCompatibilityExample
  ∷ String
  → { | InputSample }
  → NonEmptySet ForwardIncompatibility
  → Example
backwardCompatibilityExample description input forwardIncompatibilities =
  { description:
      StringNE.nes (Proxy ∷ Proxy "In this situation, ")
        `StringNE.appendString` description
        <> StringNE.nes
          ( Proxy
              ∷ Proxy
                  ". Therefore, such a change is backward compatible."
          )
  , expectedOutput: ValueSample
      { description: StringNE.nes
          (Proxy ∷ Proxy "backward compatibility")
      , sample: Backward { forwardIncompatibilities }
      }
  , input
  }

forwardCompatibilityExample
  ∷ String
  → { | InputSample }
  → NonEmptySet BackwardIncompatibility
  → Example
forwardCompatibilityExample description input backwardIncompatibilities =
  { description:
      StringNE.nes (Proxy ∷ Proxy "In this situation, ")
        `StringNE.appendString` description
        <> StringNE.nes
          ( Proxy
              ∷ Proxy
                  ". Therefore, such a change is forward compatible."
          )
  , expectedOutput: ValueSample
      { description: StringNE.nes
          (Proxy ∷ Proxy "forward compatibility")
      , sample: Forward { backwardIncompatibilities }
      }
  , input
  }

noCompatibilityExample
  ∷ String
  → { | InputSample }
  → NonEmptySet BackwardIncompatibility
  → NonEmptySet ForwardIncompatibility
  → Example
noCompatibilityExample
  description
  input
  backwardIncompatibilities
  forwardIncompatibilities =
  { description:
      StringNE.nes (Proxy ∷ Proxy "In this situation, ")
        `StringNE.appendString` description
        <> StringNE.nes
          ( Proxy
              ∷ Proxy
                  ". Therefore, such a change is not compatible."
          )
  , expectedOutput: ValueSample
      { description: StringNE.nes
          (Proxy ∷ Proxy "no compatibility")
      , sample: None
          { backwardIncompatibilities, forwardIncompatibilities }
      }
  , input
  }
