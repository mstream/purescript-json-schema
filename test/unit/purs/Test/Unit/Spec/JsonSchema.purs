module Test.Unit.Spec.JsonSchema (spec) where

import Prelude

import Data.Either (Either(..))
import Data.String.NonEmpty as StringNE
import JsonSchema (JsonSchema)
import JsonSchema as Schema
import JsonSchema.Codec.Parsing as Parsing
import JsonSchema.Gen as SchemaGen
import JsonValue (JsonValue)
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

type Input = (schema ∷ JsonSchema)
type InputSpec = (schema ∷ ValueSpec JsonSchema)
type InputSample = (schema ∷ ValueSample JsonSchema)
type Output = JsonValue
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
  , description: \{ schema: ValueSpec schemaDesc } →
      StringNE.nes (Proxy ∷ Proxy "printing ") <> schemaDesc
  , examples
  , execute: \{ schema: ValueSample schema } →
      Schema.print schema.sample
  , input:
      { schema: ValueSpec $ StringNE.nes (Proxy ∷ Proxy "JSON schema") }
  , output: ValueSpec $ StringNE.nes
      (Proxy ∷ Proxy "a JSON value representing the given JSON schema")
  , properties
  }

context ∷ ComputationContext
context = []

properties ∷ Array Property
properties =
  [ { description: StringNE.nes
        ( Proxy
            ∷ Proxy "always prints a well-formatted schema JSON"
        )
    , property: \execute → do
        schema ← genAnyJsonSchemaSample

        let
          printedJson = execute { schema }

        pure case Parsing.parseSchema printedJson of
          Left errorMessage →
            Failed $ "could not parse back the printed schema: "
              <> errorMessage
          Right _ →
            Success

    }
  ]

genAnyJsonSchemaSample ∷ Gen (ValueSample JsonSchema)
genAnyJsonSchemaSample = genValueSample
  (StringNE.nes (Proxy ∷ Proxy "any JSON schema"))
  SchemaGen.genSchema

examples ∷ Array Example
examples = []
