module Main.CLI (main) where

import Prelude

import CLI (OutputFormat(..), ProgramOutput)
import CLI as CLI
import CLI.Compat as Compat
import CLI.Diff as Diff
import CLI.Validate as Validate
import Data.Either (Either(..))
import Data.Foldable (fold)
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process as Process
import Options.Applicative (Parser, ParserInfo, ReadM, (<**>))
import Options.Applicative as O

type Options = { command ∷ Command }

data Command
  = Compat Compat.Options
  | Diff Diff.Options
  | Validate Validate.Options

optionsParser ∷ Parser Options
optionsParser = ado
  command ← commandParser
  in { command }

commandParser ∷ Parser Command
commandParser = O.subparser
  $
    O.command "compat"
      ( O.info compatParser
          (O.progDesc "Evaluate compatibility between schemata")
      )
      <>
        O.command "diff"
          ( O.info diffParser
              (O.progDesc "Calculate differences between schemata")
          )
      <> O.command "validate"
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

compatParser ∷ Parser Command
compatParser = ado
  leftSchemaPath ← O.strOption $ fold
    [ O.long "left", O.metavar "FILE" ]
  rightSchemaPath ← O.strOption $ fold
    [ O.long "right", O.metavar "FILE" ]
  outputFormat ← O.option parseOutputFormat $ fold
    [ O.long "output-format", O.short 'o', O.value Json ]
  in Compat { leftSchemaPath, outputFormat, rightSchemaPath }

diffParser ∷ Parser Command
diffParser = ado
  leftSchemaPath ← O.strOption $ fold
    [ O.long "left", O.metavar "FILE" ]
  rightSchemaPath ← O.strOption $ fold
    [ O.long "right", O.metavar "FILE" ]
  outputFormat ← O.option parseOutputFormat $ fold
    [ O.long "output-format", O.short 'o', O.value Json ]
  in Diff { leftSchemaPath, outputFormat, rightSchemaPath }

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
main = Aff.launchAff_ do
  options ← liftEffect $ O.execParser opts
  programOutput ← run options
  Console.info programOutput.stdout
  Console.error programOutput.stderr
  liftEffect $ Process.setExitCode programOutput.exitCode

opts ∷ ParserInfo Options
opts = O.info (optionsParser <**> O.helper) O.idm

run ∷ Options → Aff ProgramOutput
run { command } = Aff.catchError executeProgram fallbackProgram
  where
  fallbackProgram ∷ Error → Aff ProgramOutput
  fallbackProgram error = pure
    { exitCode: 2, stderr: Aff.message error, stdout: "" }

  executeProgram ∷ Aff ProgramOutput
  executeProgram = case command of
    Compat { leftSchemaPath, outputFormat, rightSchemaPath } → do
      Console.error $ "calculating compatibility between schemata at "
        <> leftSchemaPath
        <> " and "
        <> rightSchemaPath

      leftSchemaText ← FS.readTextFile UTF8 leftSchemaPath
      rightSchemaText ← FS.readTextFile UTF8 rightSchemaPath

      pure $ CLI.runProgram
        outputFormat
        Compat.compute
        { leftSchemaText, rightSchemaText }

    Diff { leftSchemaPath, outputFormat, rightSchemaPath } → do
      Console.error $ "calculating difference between schemata at "
        <> leftSchemaPath
        <> " and "
        <> rightSchemaPath

      leftSchemaText ← FS.readTextFile UTF8 leftSchemaPath
      rightSchemaText ← FS.readTextFile UTF8 rightSchemaPath

      pure $ CLI.runProgram
        outputFormat
        Diff.compute
        { leftSchemaText, rightSchemaText }

    Validate { jsonPath, outputFormat, schemaPath } → do
      Console.error $ "validating JSON at "
        <> jsonPath
        <> " against schema at "
        <> schemaPath

      schemaText ← FS.readTextFile UTF8 schemaPath
      jsonText ← FS.readTextFile UTF8 jsonPath

      pure $ CLI.runProgram
        outputFormat
        Validate.compute
        { jsonText, schemaText }
