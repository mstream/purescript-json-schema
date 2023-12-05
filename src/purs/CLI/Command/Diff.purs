module CLI.Command.Diff
  ( Options
  , program
  ) where

import Prelude

import CLI.Command as Command
import CLI.Program (class FileAccess, OutputFormat, ProgramOutput)
import CLI.Program as Program
import Control.Monad.Error.Class (class MonadError, liftEither)
import Data.Argonaut.Parser as AP
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Newtype (wrap)
import Data.Set as Set
import Effect.Exception (Error)
import Effect.Exception as Exception
import JsonSchema (JsonSchema)
import JsonSchema.Codec.Parsing as Parsing
import JsonSchema.Difference as Difference

type Options =
  { leftSchemaFilePath ∷ String
  , rightSchemaFilePath ∷ String
  }

program
  ∷ ∀ m
  . FileAccess m
  ⇒ MonadError Error m
  ⇒ OutputFormat
  → Options
  → m ProgramOutput
program = Command.commandProgram
  \{ leftSchemaFilePath, rightSchemaFilePath } → do
    leftSchemaText ← Program.readFileContent leftSchemaFilePath
    rightSchemaText ← Program.readFileContent rightSchemaFilePath

    leftSchema ← liftEither $ parseSchema leftSchemaText
    rightSchema ← liftEither $ parseSchema rightSchemaText
    let
      differences = Difference.calculate leftSchema rightSchema

    pure
      if Set.isEmpty differences then Right differences
      else Left differences

  where
  parseSchema ∷ String → Error \/ JsonSchema
  parseSchema s = either (Left <<< Exception.error) Right do
    json ← wrap <$> AP.jsonParser s
    Parsing.parseSchema json
