module CLI.Program
  ( expectedError
  , readFileContent
  , successfulOutput
  , unexpectedError
  , OutputFormat(..)
  , ProgramOutput
  , class FileAccess
  ) where

import Prelude

type ProgramOutput =
  { exitCode ∷ Int, stderr ∷ String, stdout ∷ String }

data OutputFormat = Json | Markdown

derive instance Eq OutputFormat

class (Monad m) ⇐ FileAccess m where
  readFileContent ∷ String → m String

successfulOutput ∷ ∀ o. (o → String) → o → ProgramOutput
successfulOutput renderOutput output =
  { exitCode: 0, stderr: "", stdout: renderOutput output }

expectedError ∷ ∀ o. (o → String) → o → ProgramOutput
expectedError renderOutput output =
  { exitCode: 1, stderr: "", stdout: renderOutput output }

unexpectedError ∷ String → ProgramOutput
unexpectedError message = { exitCode: 2, stderr: message, stdout: "" }
