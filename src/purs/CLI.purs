module CLI
  ( runProgram
  , OutputFormat(..)
  , Program
  , ProgramOutput
  , PureProgram
  ) where

import Prelude

import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode as AE
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Markdown as M
import Docs.Document (class Document)
import Docs.Document as Document

type ProgramOutput =
  { exitCode ∷ Int, stderr ∷ String, stdout ∷ String }

type Program i = i → ProgramOutput

type PureProgram i o =
  i → String \/ { expectedError ∷ Boolean, output ∷ o }

data OutputFormat = Json | Markdown

runProgram
  ∷ ∀ i o
  . Document o
  ⇒ EncodeJson o
  ⇒ OutputFormat
  → PureProgram i o
  → i
  → ProgramOutput
runProgram outputFormat compute = compute >>> case _ of
  Left errorMessage →
    { exitCode: 2, stderr: errorMessage <> "\n", stdout: "" }
  Right { expectedError, output } →
    { exitCode: if expectedError then 1 else 0
    , stderr: ""
    , stdout: renderOutput output <> "\n"
    }
  where
  renderOutput ∷ o → String
  renderOutput = case outputFormat of
    Json →
      A.stringifyWithIndent 2 <<< AE.encodeJson
    Markdown →
      M.render { maxLineLength: 72 }
        <<< M.document
        <<< Document.document
