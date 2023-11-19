module Main.CLI (main) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Main.Validate as Validate
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process as Process
import Options.Applicative (Parser, ParserInfo, ReadM, (<**>))
import Options.Applicative as O

type Options = { command ∷ Command }

data Command = Validate Validate.Options

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

parseOutputFormat ∷ ReadM Validate.OutputFormat
parseOutputFormat = O.eitherReader case _ of
  "json" →
    Right Validate.Json
  "markdown" →
    Right Validate.Markdown
  unsupportedFormat →
    Left $ "Unsupported format '" <> unsupportedFormat <> "'"

validateParser ∷ Parser Command
validateParser = ado
  schemaPath ← O.strOption $ fold
    [ O.long "schema", O.metavar "FILE" ]
  jsonPath ← O.strOption $ fold
    [ O.long "json", O.metavar "FILE" ]
  outputFormat ← O.option parseOutputFormat $ fold
    [ O.long "output-format", O.short 'o', O.value Validate.Json ]
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

    schemaText ← FS.readTextFile UTF8 schemaPath
    jsonText ← FS.readTextFile UTF8 jsonPath
    let
      { exitCode, stderr, stdout } = Validate.compute
        { jsonText, outputFormat, schemaText }

    Console.info stdout
    Console.error stderr
    liftEffect $ Process.exit' exitCode
