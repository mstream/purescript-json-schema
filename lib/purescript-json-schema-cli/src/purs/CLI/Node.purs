module CLI.Node (opts) where

import Prelude

import CLI (Command(..), Options)
import CLI.Program (OutputFormat(..))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Options.Applicative (Parser, ParserInfo, ReadM, (<**>))
import Options.Applicative as O

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
  leftSchemaFilePath ← O.strOption $ fold
    [ O.long "left", O.metavar "FILE" ]
  rightSchemaFilePath ← O.strOption $ fold
    [ O.long "right", O.metavar "FILE" ]
  outputFormat ← O.option parseOutputFormat $ fold
    [ O.long "output-format", O.short 'o', O.value Json ]
  in Compat outputFormat { leftSchemaFilePath, rightSchemaFilePath }

diffParser ∷ Parser Command
diffParser = ado
  leftSchemaFilePath ← O.strOption $ fold
    [ O.long "left", O.metavar "FILE" ]
  rightSchemaFilePath ← O.strOption $ fold
    [ O.long "right", O.metavar "FILE" ]
  outputFormat ← O.option parseOutputFormat $ fold
    [ O.long "output-format", O.short 'o', O.value Json ]
  in Diff outputFormat { leftSchemaFilePath, rightSchemaFilePath }

validateParser ∷ Parser Command
validateParser = ado
  schemaFilePath ← O.strOption $ fold
    [ O.long "schema", O.metavar "FILE" ]
  jsonFilePath ← O.strOption $ fold
    [ O.long "json", O.metavar "FILE" ]
  outputFormat ← O.option parseOutputFormat $ fold
    [ O.long "output-format", O.short 'o', O.value Json ]
  in Validate outputFormat { jsonFilePath, schemaFilePath }

opts ∷ ParserInfo Options
opts = O.info (optionsParser <**> O.helper) O.idm
