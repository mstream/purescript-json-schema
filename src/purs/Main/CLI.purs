module Main.CLI (main) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core as A
import Data.Argonaut.Parser as AP
import Data.Either (Either(..))
import Data.Foldable (fold)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import JsonSchema.Codec.Parsing as Parsing
import JsonSchema.Codec.Printing as Printing
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Options.Applicative (Parser, ParserInfo, (<**>))
import Options.Applicative as O

type Options = { command ∷ Command }

data Command = Validate ValidateOptions

type ValidateOptions =
  { jsonPath ∷ String
  , schemaPath ∷ String
  }

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

validateParser ∷ Parser Command
validateParser = ado
  jsonPath ← O.strOption $ fold
    [ O.long "json", O.metavar "FILE" ]
  schemaPath ← O.strOption $ fold
    [ O.long "schema", O.metavar "FILE" ]
  in Validate { jsonPath, schemaPath }

main ∷ Effect Unit
main = launchAff_ do
  options ← liftEffect $ O.execParser opts
  run options

opts ∷ ParserInfo Options
opts = O.info (optionsParser <**> O.helper) O.idm

run ∷ Options → Aff Unit
run { command } = case command of
  Validate { jsonPath, schemaPath } → do
    Console.info $ "validating JSON at "
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

    schema ← case Parsing.parseSchema =<< AP.jsonParser schemaText of
      Left errorMessage →
        throwError
          $ error
          $ "Failed to parse the JSON schema document: " <> errorMessage
      Right json →
        pure json

    Console.info
      $ "JSON:\n" <> A.stringify jsonValue

    Console.info
      $ "Schema:\n" <> (A.stringify $ Printing.printSchema schema)

    pure unit
