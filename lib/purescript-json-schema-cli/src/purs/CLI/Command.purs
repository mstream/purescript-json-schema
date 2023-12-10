module CLI.Command
  ( commandProgram
  ) where

import Prelude

import CLI.Program (OutputFormat(..), ProgramOutput)
import CLI.Program as Program
import Control.Monad.Error.Class (class MonadError, catchError)
import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode as AE
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Markdown as M
import Docs.Document (class Document)
import Docs.Document as Document
import Effect.Exception (Error)
import Effect.Exception as Exception

commandProgram
  ∷ ∀ i m o
  . Applicative m
  ⇒ Document o
  ⇒ EncodeJson o
  ⇒ MonadError Error m
  ⇒ ({ | i } → m (o \/ o))
  → OutputFormat
  → { | i }
  → m ProgramOutput
commandProgram program outputFormat options = catchError
  (handleProgramOutput =<< program options)
  fallback

  where
  fallback ∷ Error → m ProgramOutput
  fallback = pure <<< Program.unexpectedError <<< Exception.message

  handleProgramOutput ∷ o \/ o → m ProgramOutput
  handleProgramOutput = pure <<< either
    (Program.expectedError renderOutput)
    (Program.successfulOutput renderOutput)

  renderOutput ∷ o → String
  renderOutput = case outputFormat of
    Json →
      A.stringifyWithIndent 2 <<< AE.encodeJson
    Markdown →
      M.render { maxLineLength: 72 }
        <<< M.document
        <<< Document.document
