module Main.Validate
  ( Options
  , OutputFormat(..)
  , ProgramOutput
  , ProgramInput
  , compute
  ) where

import Prelude

import Data.Argonaut.Core as A
import Data.Argonaut.Encode as AE
import Data.Argonaut.Parser as AP
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Markdown as M
import Data.Newtype (wrap)
import Docs.Document (document)
import JsonSchema.Codec.Parsing as Parsing
import JsonSchema.Validation as Validation
import JsonValue (JsonValue(..))

type Options =
  { jsonPath ∷ String
  , outputFormat ∷ OutputFormat
  , schemaPath ∷ String
  }

data OutputFormat = Json | Markdown

type ProgramOutput =
  { exitCode ∷ Int, stderr ∷ String, stdout ∷ String }

type ProgramInput =
  { jsonText ∷ String
  , outputFormat ∷ OutputFormat
  , schemaText ∷ String
  }

compute ∷ ProgramInput → ProgramOutput
compute { jsonText, outputFormat, schemaText } = case go of
  Left errorMessage →
    { exitCode: 1, stderr: errorMessage, stdout: "" }
  Right output →
    { exitCode: 0, stderr: "", stdout: output }
  where
  go ∷ String \/ String
  go = do
    schema ←
      case
        Parsing.parseSchema =<< (wrap <$> AP.jsonParser schemaText)
        of
        Left errorMessage →
          Left $ "Failed to parse the JSON schema document: "
            <> errorMessage
        Right json →
          Right json

    jsonValue ← case AP.jsonParser jsonText of
      Left errorMessage →
        Left $ "Failed to parse the JSON value document: "
          <> errorMessage
      Right json →
        Right json

    let
      output = case outputFormat of
        Json →
          A.stringifyWithIndent 2 $ AE.encodeJson $ JsonValue jsonValue
            `Validation.validateAgainst` schema
        Markdown →
          M.render { maxLineLength: 72 }
            $ M.document
            $ document
            $ JsonValue jsonValue `Validation.validateAgainst` schema

    Right $ output <> "\n"
