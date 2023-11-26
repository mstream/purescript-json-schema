module CLI.Diff
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
import JsonSchema.Difference (Difference)
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

type ProgramOutput = Set Difference

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

  Right
    { expectedError: not $ Set.isEmpty differences
    , output: differences
    }
  where
  parseSchema ∷ String → String \/ JsonSchema
  parseSchema s = do
    json ← wrap <$> AP.jsonParser s
    Parsing.parseSchema json
