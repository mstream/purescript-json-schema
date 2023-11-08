module Main.CLI (main) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core as A
import Data.Argonaut.Encode as AE
import Data.Argonaut.Parser as AP
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Markdown as M
import Data.Newtype (wrap)
import Docs.Document (document)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import JsonSchema.Codec.Parsing as Parsing
import JsonSchema.Validation as Validation
import JsonValue (JsonValue(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Options.Applicative (Parser, ParserInfo, ReadM, (<**>))
import Options.Applicative as O

type Options = { command ∷ Command }

data Command = Validate ValidateOptions

type ValidateOptions =
  { jsonPath ∷ String
  , outputFormat ∷ OutputFormat
  , schemaPath ∷ String
  }

data OutputFormat = Json | Markdown

optionsParser ∷ Parser Options
optionsParser = ado
  command ← commandParser
  in { command }

commandParser ∷ Parser Command
commandParser = O.subparser
  $ O.command "validate"
      ( O.info validateParser
          (O.progDesc "Validate a JSON value against a schema")
      )

parseOutputFormat ∷ ReadM OutputFormat
parseOutputFormat = O.eitherReader case _ of
  "json" →
    Right Json
  "markdown" →
    Right Markdown
  unsupportedFormat →
    Left $ "Unsupported format '" <> unsupportedFormat <> "'"

validateParser ∷ Parser Command
validateParser = ado
  schemaPath ← O.strOption $ fold
    [ O.long "schema", O.metavar "FILE" ]
  jsonPath ← O.strOption $ fold
    [ O.long "json", O.metavar "FILE" ]
  outputFormat ← O.option parseOutputFormat $ fold
    [ O.long "output-format", O.short 'o', O.value Json ]
  in Validate { jsonPath, outputFormat, schemaPath }

main ∷ Effect Unit
main = launchAff_ do
  options ← liftEffect $ O.execParser opts
  run options

opts ∷ ParserInfo Options
opts = O.info (optionsParser <**> O.helper) O.idm

run ∷ Options → Aff Unit
run { command } = case command of
  Validate { jsonPath, outputFormat, schemaPath } → do
    Console.error $ "validating JSON at "
      <> jsonPath
      <> " against schema at "
      <> schemaPath

    jsonText ← FS.readTextFile UTF8 jsonPath

    jsonValue ← case AP.jsonParser jsonText of
      Left errorMessage →
        throwError
          $ error
          $ "Failed to parse the JSON value document: " <> errorMessage
      Right json →
        pure json

    schemaText ← FS.readTextFile UTF8 schemaPath

    schema ←
      case
        Parsing.parseSchema =<< (wrap <$> AP.jsonParser schemaText)
        of
        Left errorMessage →
          throwError
            $ error
            $ "Failed to parse the JSON schema document: " <>
                errorMessage
        Right json →
          pure json

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

    Console.info $ output <> "\n"
    pure unit
