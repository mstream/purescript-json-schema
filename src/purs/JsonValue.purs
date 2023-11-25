module JsonValue (JsonValue(..)) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Encode (class EncodeJson)
import Data.Markdown (CodeLanguage(..))
import Data.Markdown as M
import Data.Newtype (class Newtype)
import Data.NonEmpty as NE
import Data.String (Pattern(..))
import Data.String as String
import Docs.Document (class Document)

newtype JsonValue = JsonValue Json

derive instance Newtype JsonValue _
derive newtype instance EncodeJson JsonValue
derive newtype instance Eq JsonValue
derive newtype instance Ord JsonValue

instance Show JsonValue where
  show (JsonValue json) = A.stringify json

instance Document JsonValue where
  document (JsonValue json) = NE.singleton
    $ M.codeBlock Json
    $ String.split (Pattern "\n")
    $ A.stringifyWithIndent 2
    $ json
