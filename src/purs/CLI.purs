module CLI (runProgram, OutputFormat(..), Program, ProgramOutput) where

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

data OutputFormat = Json | Markdown

runProgram
  ∷ ∀ i o
  . Document o
  ⇒ EncodeJson o
  ⇒ OutputFormat
  → (i → String \/ o)
  → i
  → ProgramOutput
runProgram outputFormat compute = compute >>> case _ of
  Left errorMessage →
    { exitCode: 1, stderr: errorMessage <> "\n", stdout: "" }
  Right output →
    { exitCode: 0, stderr: "", stdout: renderOutput output <> "\n" }
  where
  renderOutput ∷ o → String
  renderOutput = case outputFormat of
    Json →
      A.stringifyWithIndent 2 <<< AE.encodeJson
    Markdown →
      M.render { maxLineLength: 72 }
        <<< M.document
        <<< Document.document
