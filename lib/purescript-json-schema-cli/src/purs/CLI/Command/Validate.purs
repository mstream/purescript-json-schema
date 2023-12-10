module CLI.Command.Validate (Options, program) where

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
import JsonSchema.Validation as Validation
import JsonValue (JsonValue)

type Options =
  { jsonFilePath ∷ String
  , schemaFilePath ∷ String
  }

program
  ∷ ∀ m
  . FileAccess m
  ⇒ MonadError Error m
  ⇒ OutputFormat
  → Options
  → m ProgramOutput
program = Command.commandProgram \{ jsonFilePath, schemaFilePath } → do
  schemaText ← Program.readFileContent schemaFilePath
  jsonText ← Program.readFileContent jsonFilePath

  schema ← liftEither $ parseSchema schemaText
  json ← liftEither $ parseJson jsonText

  let
    violations = json `Validation.validateAgainst` schema

  pure
    if Set.isEmpty violations then Right violations
    else Left violations

  where
  parseJson ∷ String → Error \/ JsonValue
  parseJson s = either
    (Left <<< Exception.error)
    Right
    (map wrap $ AP.jsonParser s)

  parseSchema ∷ String → Error \/ JsonSchema
  parseSchema s = either (Left <<< Exception.error) Right do
    json ← wrap <$> AP.jsonParser s
    Parsing.parseSchema json
