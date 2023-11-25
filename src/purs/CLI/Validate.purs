module CLI.Validate
  ( Options
  , ProgramInput
  , compute
  ) where

import Prelude

import CLI (OutputFormat)
import Data.Argonaut.Parser as AP
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Newtype (wrap)
import Data.Set (Set)
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

compute ∷ ProgramInput → String \/ Set Violation
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

  Right $ jsonValue `Validation.validateAgainst` schema
  where
  parseJson ∷ String → String \/ JsonValue
  parseJson = map wrap <<< AP.jsonParser

  parseSchema ∷ String → String \/ JsonSchema
  parseSchema s = do
    json ← wrap <$> AP.jsonParser s
    Parsing.parseSchema json
