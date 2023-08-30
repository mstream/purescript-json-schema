module Test.Spec.JsonSchema.Diff (doc, spec) where

import Prelude

import Data.Argonaut.Core as A
import Data.Foldable (foldMap, traverse_)
import Data.List (List(..), (:))
import Data.Markdown (CodeBlockType(..), Document)
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Docs.Types (Doc)
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.Codec.Printing as Printing
import JsonSchema.Diff (Difference, DifferenceType(..))
import JsonSchema.Diff as Diff
import JsonSchema.Gen as SchemaGen
import JsonSchema.SchemaPath (SchemaPathSegment(..))
import Test.Spec (describe)
import Test.Types
  ( Computation
  , Example
  , ExpectedOutput
  , Input
  , Property
  , TestLength(..)
  , TestSpec
  )
import Test.Utils
  ( exampleTestCase
  , exampleTitle
  , propertyTest
  , propertyTitle
  )

type DiffInput =
  { nextSchema ∷ JsonSchema
  , previousSchema ∷ JsonSchema
  }

type DiffOutput = Set Difference

type DiffExample = Example DiffInput DiffOutput
type DiffProperty = Property DiffInput DiffOutput

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

spec ∷ TestSpec
spec = describe "Diff" do
  describe "calculate" do
    traverse_ exampleTestCase examples

    propertyTest
      Long
      { computation
      , expectedOutput:
          { description: "no differences"
          , value: Set.empty
          }
      , input:
          { description: "two identical schemata"
          , gen: do
              schema ← SchemaGen.genSchema
              pure { nextSchema: schema, previousSchema: schema }
          }
      , renderInput: \{ description } → [ M.paragraph description ]
      , renderOutput
      }

properties ∷ Array DiffProperty
properties =
  [ { computation:
        { description: "comparing"
        , execute: \{ nextSchema, previousSchema } → Diff.calculate
            previousSchema
            nextSchema
        }
    , expectedOutput:
        { description: "no differences"
        , value: Set.empty
        }
    , input:
        { description: "two identical schemata"
        , gen: do
            schema ← SchemaGen.genSchema
            pure { nextSchema: schema, previousSchema: schema }
        }
    , renderInput: \{ description } → [ M.paragraph description ]
    , renderOutput
    }
  ]

examples ∷ Array DiffExample
examples =
  [ scenario
      "When two identical schemata are compared, no difference should be found."
      { description: "identical schemata"
      , value:
          { nextSchema: BooleanSchema false
          , previousSchema: BooleanSchema false
          }
      }
      Set.empty
  , scenario
      "Any change in expected JSON value type should be accounted as a difference."
      { description:
          "schema with expected type of null to schema with expected type of boolean"
      , value:
          { nextSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { typeKeyword = Just $ Set.singleton JsonBoolean }
          , previousSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { typeKeyword = Just $ Set.singleton JsonNull }
          }
      }
      ( Set.singleton
          { differenceType: TypeChange
              (Just $ Set.singleton JsonNull)
              (Just $ Set.singleton JsonBoolean)
          , path: TypeKeyword : Nil
          }
      )
  , scenario
      "changes of multipleOf keyword should be reported"
      { description:
          "schema expecting multiples of 2 to schema expecting multiples of 4"
      , value:
          { nextSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { multipleOf = Just 4.0
                  }
          , previousSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { multipleOf = Just 2.0
                  }
          }
      }
      ( Set.singleton
          { differenceType: MultipleOfChange (Just 2.0) (Just 4.0)
          , path: MultipleOf : Nil
          }
      )
  , scenario
      "changes of exclusiveMaximum keyword should be reported"
      { description:
          "schema expecting lower (exclusive) maximum value to schema expecting higher (exclusive) maximum value"
      , value:
          { nextSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { exclusiveMaximum = Just 4.0
                  }
          , previousSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { exclusiveMaximum = Just 2.0
                  }
          }
      }
      ( Set.singleton
          { differenceType: ExclusiveMaximumChange
              (Just 2.0)
              (Just 4.0)
          , path: ExclusiveMaximum : Nil
          }
      )
  , scenario
      "changes of minimum keyword should be reported"
      { description:
          "schema expecting lower (exclusive) minimum value to schema expecting higher (exclusive) minimum value"
      , value:
          { nextSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { exclusiveMinimum = Just 4.0
                  }
          , previousSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { exclusiveMinimum = Just 2.0
                  }
          }
      }
      ( Set.singleton
          { differenceType: ExclusiveMinimumChange (Just 2.0) (Just 4.0)
          , path: ExclusiveMinimum : Nil
          }
      )
  , scenario
      "changes of maximum keyword should be reported"
      { description:
          "schema expecting lower (inclusive) maximum value to schema expecting higher (inclusive) maximum value"
      , value:
          { nextSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { maximum = Just 4.0
                  }
          , previousSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { maximum = Just 2.0
                  }
          }
      }
      ( Set.singleton
          { differenceType: MaximumChange (Just 2.0) (Just 4.0)
          , path: Maximum : Nil
          }
      )
  , scenario
      "changes of minimum keyword should be reported"
      { description:
          "schema expecting lower (inclusive) minimum value to schema expecting higher (inclusive) minimum value"
      , value:
          { nextSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { minimum = Just 4.0
                  }
          , previousSchema: ObjectSchema
              $ Schema.defaultKeywords
                  { minimum = Just 2.0
                  }
          }
      }
      ( Set.singleton
          { differenceType: MinimumChange (Just 2.0) (Just 4.0)
          , path: Minimum : Nil
          }
      )
  ]

scenario
  ∷ String
  → Input DiffInput
  → Set Difference
  → DiffExample
scenario description input expectedDifferences =
  { computation
  , description
  , expectedOutput:
      { description: "differences", value: expectedDifferences }
  , input
  , renderInput
  , renderOutput
  }

renderInput ∷ Input DiffInput → Document
renderInput { value: { nextSchema, previousSchema } } =
  [ M.paragraph "*Previous JSON schema:*"
  , M.codeBlock Json
      $ (A.stringifyWithIndent 2 <<< Printing.printSchema)
          previousSchema
  , M.paragraph "*Next JSON schema:*"
  , M.codeBlock Json
      $ (A.stringifyWithIndent 2 <<< Printing.printSchema) nextSchema
  ]

renderOutput ∷ ExpectedOutput (Set Difference) → Document
renderOutput { value: differences } =
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

computation ∷ Computation DiffInput DiffOutput
computation =
  { description: "comparing"
  , execute: \{ nextSchema, previousSchema } → Diff.calculate
      previousSchema
      nextSchema
  }
