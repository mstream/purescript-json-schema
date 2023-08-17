module JsonSchema.Validation (validateAgainst) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import JsonSchema (JsonSchema(..), JsonValueType(..), Keywords)

type Violation = { description ∷ String, path ∷ String }

validateAgainst ∷ Json → JsonSchema → Set Violation
validateAgainst json schema = case schema of
  BooleanSchema bool →
    if bool then Set.empty
    else Set.singleton { description: "invalid JSON value", path: "?" }
  ObjectSchema keywords →
    validateAgainstObjectSchema json keywords

validateAgainstObjectSchema
  ∷ Json → Keywords → Set Violation
validateAgainstObjectSchema json keywords =
  notViolations <> typeKeywordViolations
  where
  notViolations = case keywords.not of
    Just schema →
      if Set.isEmpty $ validateAgainst json schema then Set.singleton
        { description: "JSON value matches schema when it should not."
        , path: "?"
        }
      else Set.empty
    Nothing →
      Set.empty

  typeKeywordViolations ∷ Set Violation
  typeKeywordViolations = maybe
    Set.empty
    (validateTypeKeyword json)
    keywords.typeKeyword

validateTypeKeyword ∷ Json → Set JsonValueType → Set Violation
validateTypeKeyword json allowedJsonValueTypes =
  if jsonValueType `Set.member` allowedJsonValueTypes then Set.empty
  else Set.singleton { description: "", path: "?" }
  where
  jsonValueType ∷ JsonValueType
  jsonValueType = A.caseJson
    (const JsonNull)
    (const JsonBoolean)
    ( \x →
        if (Int.toNumber $ Int.trunc x) == x then JsonInteger
        else JsonNumber
    )
    (const JsonString)
    (const JsonArray)
    (const JsonObject)
    json
