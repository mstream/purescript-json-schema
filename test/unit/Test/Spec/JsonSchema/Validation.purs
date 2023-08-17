module Test.Spec.JsonSchema.Validation (spec) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Argonaut.Core as A
import Data.Argonaut.Gen as AGen
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import JsonSchema (JsonSchema(..), JsonValueType(..))
import JsonSchema as Schema
import JsonSchema.Codec.Printing as Printing
import JsonSchema.Gen as SchemaGen
import JsonSchema.Validation as Validation
import Test.QuickCheck (Result(..))
import Test.Spec (describe)
import Test.Types (TestSpec)
import Test.Utils (TestLength(..), failWithDetails, generativeTestCase)

spec ∷ TestSpec
spec = describe "Validation" do

  describe "validateAgainst" do

    generativeTestCase Short
      "null type accepts only null JSON values"
      do
        nonNullJsons ← Gen.unfoldable
          $ AGen.genJson `Gen.suchThat` (not A.isNull)

        nullJsons ← Gen.unfoldable
          $ pure A.jsonNull

        let
          schema = ObjectSchema $ Schema.defaultKeywords
            { typeKeyword = Just $ Set.fromFoldable [ JsonNull ] }
          violationsLists = (_ `Validation.validateAgainst` schema)
            <$> (nonNullJsons <> nullJsons)
          failedValidations = List.filter
            (not Set.isEmpty)
            violationsLists
          successfulValidations = List.filter
            Set.isEmpty
            violationsLists
          actualFailures = List.length failedValidations
          actualSuccesses = List.length successfulValidations
          expectedFailures = List.length nonNullJsons
          expectedSuccesses = List.length nullJsons

        pure
          if
            actualFailures == expectedFailures
              && actualSuccesses == expectedSuccesses then
            Success
          else failWithDetails
            "number of failed and successful validation results do not match expectations"
            { actualFailures
            , actualSuccesses
            , expectedFailures
            , expectedSuccesses
            , failedValidations
            , nonNullJsons: A.stringify <$> nonNullJsons
            , nullJsons: A.stringify <$> nullJsons
            , successfulValidations
            }

    generativeTestCase Long
      "negated schema should yield negated validation result"
      do
        originalSchema ← SchemaGen.genSchema
        json ← AGen.genJson

        let
          negatedSchema = ObjectSchema
            $ Schema.defaultKeywords { not = Just originalSchema }
          originalSchemaViolations = Set.toUnfoldable
            $ json `Validation.validateAgainst` originalSchema
          negatedSchemaViolations = Set.toUnfoldable
            $ json `Validation.validateAgainst` negatedSchema

        pure case originalSchemaViolations of
          [] →
            case negatedSchemaViolations of
              [] →
                failWithDetails
                  "validation has passed for both original and negated schemata"
                  { negatedSchema: A.stringify
                      $ Printing.printSchema negatedSchema
                  , originalSchema: A.stringify
                      $ Printing.printSchema originalSchema
                  }

              _ →
                Success
          _ →
            case negatedSchemaViolations of
              [] →
                Success
              _ →
                failWithDetails
                  "validation has failed for both original and negated schemata"
                  { negatedSchema: A.stringify
                      $ Printing.printSchema negatedSchema
                  , negatedSchemaViolations
                  , originalSchema: A.stringify
                      $ Printing.printSchema originalSchema
                  , originalSchemaViolations
                  }
