module Test.Unit.Spec.JsonSchema.Difference (spec) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Foldable (maximum)
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
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen)
import Test.Unit.Computation
  ( ComputationContext
  , ComputationExample
  , ComputationProperty
  , ComputationSpec
  , ValueSample(..)
  , ValueSpec(..)
  )
import Test.Unit.Utils (genValueSample)
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
      \{ newSchema: ValueSpec newSchemaDesc
       , oldSchema: ValueSpec oldSchemaDesc
       } →
        StringNE.nes (Proxy ∷ Proxy "calculating differences between ")
          <> oldSchemaDesc
          <> StringNE.nes (Proxy ∷ Proxy " and ")
          <> newSchemaDesc
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
      (Proxy ∷ Proxy "differences between old and new schema")
  , properties
  }

context ∷ ComputationContext
context =
  [ M.paragraph $
      M.text
        "Calculating JSON Schema Difference is a process used to identify the changes between two JSON schemata."
        `ArrayNE.cons'`
          [ M.lineBreak
          , M.text
              "It is used to to see what has been added, removed, or changed."
          , M.lineBreak
          , M.text
              "This is useful for tracking changes over time, understanding the impact of changes, and managing versions of a schema."
          , M.lineBreak
          , M.text
              "It can also be used to generate a diff report or to automate the process of updating dependent systems or documentation when a schema changes."
          ]
  ]

properties ∷ Array Property
properties =
  [ { description: "comparing identical schemata yields no differences"
    , property: \execute → do
        schema ← genAnyJsonSchemaSample

        let
          differences = execute { newSchema: schema, oldSchema: schema }

        pure
          if Set.isEmpty differences then Success
          else Failed
            $ "the comparison produced differences: "
                <> show differences
    }
  ]

genAnyJsonSchemaSample ∷ Gen (ValueSample JsonSchema)
genAnyJsonSchemaSample = genValueSample
  (StringNE.nes (Proxy ∷ Proxy "any JSON schema"))
  SchemaGen.genSchema

examples ∷ Array Example
examples =
  [ example
      "comparing two identical schemata yields no differences"
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
      ( ValueSample
          { description: StringNE.nes (Proxy ∷ Proxy "no differences")
          , sample: Set.empty
          }
      )
  , example
      "Any change in expected JSON value type should be considered a difference."
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
  , example
      "Any change of multipleOf keyword should be considered a difference."
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
  , example
      "Any change of maximum inclusive keyword value should be considered a difference."
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
  , example
      "Any change of maximum exclusive keyword value should be considered a difference."
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
  , example
      "Any change of minimum inclusive keyword value should be considered a difference."
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
  , example
      "Any change of minimum exclusive keyword value should be considered a difference."
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

example ∷ String → { | InputSample } → ValueSample Output → Example
example description input expectedOutput =
  { description, input, expectedOutput }
