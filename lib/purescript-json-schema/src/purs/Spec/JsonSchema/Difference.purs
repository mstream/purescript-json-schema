module Test.Unit.Spec.JsonSchema.Difference (spec) where

import Prelude

import Computation
  ( ComputationContext
  , ComputationExample
  , ComputationProperty
  , ComputationSpec
  , ValueSample(..)
  , ValueSpec(..)
  )
import Control.Monad.Gen as Gen
import Data.Array.NonEmpty as ArrayNE
import Data.List (List(..), (:))
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String.NonEmpty as StringNE
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.Difference (Difference(..), DifferenceType(..))
import JsonSchema.Difference as Difference
import JsonSchema.Gen as SchemaGen
import JsonSchema.SchemaPath (SchemaPathSegment(..))
import Show.NonEmpty (show1)
import Test.Computation.Utils (genValueSample)
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy(..))

type Input = (newSchema ∷ JsonSchema, oldSchema ∷ JsonSchema)

type InputSpec =
  (newSchema ∷ ValueSpec JsonSchema, oldSchema ∷ ValueSpec JsonSchema)

type InputSample =
  ( newSchema ∷ ValueSample JsonSchema
  , oldSchema ∷ ValueSample JsonSchema
  )

type Output = Set Difference
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
  , description:
      \outputSpec { newSchema: newSchemaSpec, oldSchema: oldSchemaSpec } →
        StringNE.nes (Proxy ∷ Proxy "calculating ")
          <> show1 outputSpec
          <> StringNE.nes (Proxy ∷ Proxy " based on ")
          <> show1 oldSchemaSpec
          <> StringNE.nes (Proxy ∷ Proxy " and ")
          <> show1 newSchemaSpec
  , examples
  , execute:
      \{ newSchema: ValueSample newSchema
       , oldSchema: ValueSample oldSchema
       } →
        Difference.calculate oldSchema.sample newSchema.sample
  , input:
      { newSchema: ValueSpec $ StringNE.nes
          (Proxy ∷ Proxy "new JSON schema")
      , oldSchema: ValueSpec $ StringNE.nes
          (Proxy ∷ Proxy "old JSON schema")
      }
  , output: ValueSpec $ StringNE.nes
      (Proxy ∷ Proxy "differences between schemata")
  , properties
  }

context ∷ ComputationContext
context =
  [ M.paragraph
      $
        ( M.text
            $
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "Calculating JSON Schema Difference is a process used to identify the changes between two JSON schemata."
                )
        )
          `ArrayNE.cons'`
            [ M.lineBreak
            , M.text $ StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "It is used to to see what has been added, removed, or changed."
                )
            , M.lineBreak
            , M.text $ StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "This is useful for tracking changes over time, understanding the impact of changes, and managing versions of a schema."
                )
            , M.lineBreak
            , M.text $ StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "It can also be used to generate a diff report or to automate the process of updating dependent systems or documentation when a schema changes."
                )
            ]
  ]

properties ∷ Array Property
properties =
  [ { description: StringNE.nes
        ( Proxy
            ∷ Proxy "comparing identical schemata yields no differences"
        )
    , property: \execute → do
        schemaSample ← genAnyJsonSchemaSample

        let
          differences = execute
            { newSchema: schemaSample, oldSchema: schemaSample }

        pure
          if Set.isEmpty differences then Success
          else Failed
            $ "the comparison produced differences: "
                <> show differences
    }
  , { description: StringNE.nes
        ( Proxy
            ∷ Proxy "comparing different schemata yields differences"
        )
    , property: \execute → do
        schemaSample@(ValueSample { sample }) ← genAnyJsonSchemaSample

        differentSchema ← SchemaGen.genSchema
          `Gen.suchThat` (_ /= sample)

        let
          differentSchemaSample = ValueSample
            { description: StringNE.nes
                (Proxy ∷ Proxy "a different schema")
            , sample: differentSchema
            }

          differences = execute
            { newSchema: differentSchemaSample
            , oldSchema: schemaSample
            }

        pure
          if Set.isEmpty differences then
            Failed $ "the comparison produced no differences:\n"
              <> show { differentSchemaSample, schemaSample }
          else Success
    }
  ]

genAnyJsonSchemaSample ∷ Gen (ValueSample JsonSchema)
genAnyJsonSchemaSample = genValueSample
  (StringNE.nes (Proxy ∷ Proxy "a JSON schema"))
  SchemaGen.genSchema

examples ∷ Array Example
examples =
  [ noDifferencesExample
      "two identical schemata"
      { newSchema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "some schema")
          , sample: BooleanSchema false
          }
      , oldSchema: ValueSample
          { description: StringNE.nes
              ( Proxy ∷ Proxy "same schema"
              )
          , sample: BooleanSchema false
          }
      }
  , differencesExample
      "expected JSON value type "
      { newSchema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "JSON schema accepting only other type")
          , sample: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.singleton JsonBoolean }
          }
      , oldSchema: ValueSample
          { description: StringNE.nes
              (Proxy ∷ Proxy "JSON schema accepting only some type")
          , sample: ObjectSchema $ Schema.defaultKeywords
              { typeKeyword = Just $ Set.singleton JsonNull }
          }
      }
      ( ValueSample
          { description:
              StringNE.nes
                ( Proxy
                    ∷ Proxy
                        "a change in accepted value type"
                )
          , sample: Set.singleton $ Difference
              { differenceType: TypeChange
                  (Just $ Set.singleton JsonNull)
                  (Just $ Set.singleton JsonBoolean)
              , path: TypeKeyword : Nil
              }
          }
      )
  , differencesExample
      "multipleOf keyword"
      { newSchema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON schema accepting only multiples of other number"
              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { multipleOf = Just 4.0 }
          }
      , oldSchema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON schema accepting only multiples of a some number"
              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { multipleOf = Just 2.0 }
          }
      }
      ( ValueSample
          { description:
              StringNE.nes
                (Proxy ∷ Proxy "a change in accepted value factor")
          , sample: Set.singleton $ Difference
              { differenceType: MultipleOfChange (Just 2.0) (Just 4.0)
              , path: MultipleOf : Nil
              }
          }
      )
  , differencesExample
      "maximum inclusive keyword value "
      { newSchema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON schema accepting only number less than or equal to other number"
              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { maximum = Just 4.0 }
          }
      , oldSchema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy

                      "JSON schema accepting only number less than or equal to some number"
              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { maximum = Just 2.0 }
          }
      }
      ( ValueSample
          { description:
              StringNE.nes
                (Proxy ∷ Proxy "a change in inclusive maximum value")
          , sample: Set.singleton $ Difference
              { differenceType: MaximumChange (Just 2.0) (Just 4.0)
              , path: Maximum : Nil
              }
          }
      )
  , differencesExample
      "maximum exclusive keyword value "
      { newSchema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON schema accepting only number less than other number"
              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { exclusiveMaximum = Just 4.0 }
          }
      , oldSchema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON schema accepting only number less than some number"

              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { exclusiveMaximum = Just 2.0 }
          }
      }
      ( ValueSample
          { description:
              StringNE.nes
                (Proxy ∷ Proxy "a change in exclusive maximum value")
          , sample: Set.singleton $ Difference
              { differenceType: ExclusiveMaximumChange (Just 2.0)
                  (Just 4.0)
              , path: ExclusiveMaximum : Nil
              }
          }
      )
  , differencesExample
      "minimum inclusive keyword value"
      { newSchema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON schema accepting only number greater than or equal to other number"
              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { minimum = Just 4.0 }
          }
      , oldSchema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON schema accepting only number greater than or equal to some number"
              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { minimum = Just 2.0 }
          }
      }
      ( ValueSample
          { description:
              StringNE.nes
                (Proxy ∷ Proxy "a change in inclusive minimum value")
          , sample: Set.singleton $ Difference
              { differenceType: MinimumChange (Just 2.0) (Just 4.0)
              , path: Minimum : Nil
              }
          }
      )
  , differencesExample
      "minimum exclusive keyword value "
      { newSchema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON schema accepting only number greater than other number"
              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { exclusiveMinimum = Just 4.0 }
          }
      , oldSchema: ValueSample
          { description: StringNE.nes
              ( Proxy
                  ∷ Proxy
                      "JSON schema accepting only number greater than some number"

              )
          , sample: ObjectSchema $ Schema.defaultKeywords
              { exclusiveMinimum = Just 2.0 }
          }
      }
      ( ValueSample
          { description:
              StringNE.nes
                (Proxy ∷ Proxy "a change in exclusive minimum value")
          , sample: Set.singleton $ Difference
              { differenceType: ExclusiveMinimumChange (Just 2.0)
                  (Just 4.0)
              , path: ExclusiveMinimum : Nil
              }
          }
      )

  ]

noDifferencesExample ∷ String → { | InputSample } → Example
noDifferencesExample schemataDescription input =
  { description:
      StringNE.nes (Proxy ∷ Proxy "Comparison of ")
        `StringNE.appendString` schemataDescription
        <> StringNE.nes (Proxy ∷ Proxy " yields no differences.")
  , input
  , expectedOutput: ValueSample
      { description: StringNE.nes (Proxy ∷ Proxy "no differences")
      , sample: Set.empty
      }
  }

differencesExample
  ∷ String → { | InputSample } → ValueSample Output → Example
differencesExample changeDescription input expectedOutput =
  { description:
      StringNE.nes (Proxy ∷ Proxy "Any change of ")
        `StringNE.appendString` changeDescription
        <> StringNE.nes
          (Proxy ∷ Proxy " is considered a difference.")
  , input
  , expectedOutput
  }
