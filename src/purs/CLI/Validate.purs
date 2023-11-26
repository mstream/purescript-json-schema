module CLI.Validate
  ( Options
  , ProgramInput
  , compute
  ) where

import Prelude

import CLI (OutputFormat, PureProgram)
import Data.Argonaut.Parser as AP
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set as Set
import JsonSchema (JsonSchema)
import JsonSchema.Codec.Parsing as Parsing
import JsonSchema.Validation (Violation)
import JsonSchema.Validation as Validation
import JsonValue (JsonValue)

type Options =
  { jsonPath ∷ String
  , outputFormat ∷ OutputFormat
  , schemaPath ∷ String
  }

type ProgramInput =
  { jsonText ∷ String
  , schemaText ∷ String
  }

type ProgramOutput = Set Violation

compute ∷ PureProgram ProgramInput ProgramOutput
compute { jsonText, schemaText } = do
  schema ← case parseSchema schemaText of
    Left errorMessage →
      Left $ "Failed to parse the JSON schema document: "
        <> errorMessage
    Right schema →
      Right schema

  jsonValue ← case parseJson jsonText of
    Left errorMessage →
      Left $ "Failed to parse the JSON value document: "
        <> errorMessage
    Right json →
      Right json

  let
    violations = jsonValue `Validation.validateAgainst` schema

  Right
    { expectedError: not $ Set.isEmpty violations, output: violations }
  where
  parseJson ∷ String → String \/ JsonValue
  parseJson = map wrap <<< AP.jsonParser

  parseSchema ∷ String → String \/ JsonSchema
  parseSchema s = do
    json ← wrap <$> AP.jsonParser s
    Parsing.parseSchema json
