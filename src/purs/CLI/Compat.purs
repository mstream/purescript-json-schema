module CLI.Compat
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
import JsonSchema (JsonSchema)
import JsonSchema.Codec.Parsing as Parsing
import JsonSchema.Compatibility (Compatibility(..))
import JsonSchema.Compatibility as Compatibility
import JsonSchema.Difference as Difference

type Options =
  { leftSchemaPath ∷ String
  , outputFormat ∷ OutputFormat
  , rightSchemaPath ∷ String
  }

type ProgramInput =
  { leftSchemaText ∷ String
  , rightSchemaText ∷ String
  }

type ProgramOutput = Compatibility

compute ∷ PureProgram ProgramInput ProgramOutput
compute { leftSchemaText, rightSchemaText } = do
  leftSchema ← case parseSchema leftSchemaText of
    Left errorMessage →
      Left $ "Failed to parse the left JSON schema document: "
        <> errorMessage
    Right schema →
      Right schema

  rightSchema ← case parseSchema rightSchemaText of
    Left errorMessage →
      Left $ "Failed to parse the right JSON schema document: "
        <> errorMessage
    Right schema →
      Right schema

  let
    differences = Difference.calculate leftSchema rightSchema

  Right case Compatibility.calculate differences of
    Backward details →
      { expectedError: true, output: Backward details }
    Forward details →
      { expectedError: true, output: Forward details }
    Full →
      { expectedError: false, output: Full }
    None details →
      { expectedError: true, output: None details }
  where
  parseSchema ∷ String → String \/ JsonSchema
  parseSchema s = do
    json ← wrap <$> AP.jsonParser s
    Parsing.parseSchema json
