module JsonSchema.Validation (validateAgainst) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Int as Int
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set as Set
import JsonSchema
  ( JsonArraySchemaSpec
  , JsonIntegerSchemaSpec
  , JsonNumberSchemaSpec
  , JsonObjectSchemaSpec
  , JsonSchema(..)
  , JsonStringSchemaSpec
  , ObjectFormJsonSchemaSpec(..)
  )

validateAgainst ∷ Json → JsonSchema → Set String
validateAgainst json schema = case schema of
  BooleanFormJsonSchema bool →
    if bool then Set.empty else Set.singleton "invalid JSON value"
  ObjectFormJsonSchema spec →
    validateAgainstObjectFormSchema json spec

validateAgainstObjectFormSchema
  ∷ Json → ObjectFormJsonSchemaSpec → Set String
validateAgainstObjectFormSchema json objectFormSpec =
  case objectFormSpec of
    JsonArraySchema spec →
      validateAgainstArraySchema json spec
    JsonBooleanSchema →
      validateAgainstBooleanSchema json
    JsonIntegerSchema spec →
      validateAgainstIntegerSchema json spec
    JsonEmptySchema →
      Set.empty
    JsonNullSchema →
      validateAgainstNullSchema json
    JsonNumberSchema spec →
      validateAgainstNumberSchema json spec
    JsonObjectSchema spec →
      validateAgainstObjectSchema json spec
    JsonStringSchema spec →
      validateAgainstStringSchema json spec

validateAgainstArraySchema ∷ Json → JsonArraySchemaSpec → Set String
validateAgainstArraySchema json spec =
  A.caseJsonArray
    (Set.singleton "Not an array")
    ( \jsons →
        Set.empty
    )
    json

validateAgainstBooleanSchema ∷ Json → Set String
validateAgainstBooleanSchema json =
  if A.isBoolean json then Set.empty else Set.singleton "Not a boolean."

validateAgainstIntegerSchema ∷ Json → JsonIntegerSchemaSpec → Set String
validateAgainstIntegerSchema json spec =
  A.caseJsonNumber
    (Set.singleton "Not an integer")
    ( \x →
        if isNothing $ Int.fromNumber x then
          Set.singleton "Not an integer"
        else
          Set.empty
    )
    json

validateAgainstNullSchema ∷ Json → Set String
validateAgainstNullSchema json =
  if A.isNull json then Set.empty else Set.singleton "Not a null."

validateAgainstNumberSchema ∷ Json → JsonNumberSchemaSpec → Set String
validateAgainstNumberSchema json spec =
  if A.isNumber json then Set.empty else Set.singleton "Not a number."

validateAgainstObjectSchema ∷ Json → JsonObjectSchemaSpec → Set String
validateAgainstObjectSchema json spec =
  A.caseJsonObject
    (Set.singleton "Not an object")
    ( \object →
        Set.empty
    )
    json

validateAgainstStringSchema ∷ Json → JsonStringSchemaSpec → Set String
validateAgainstStringSchema json spec =
  if A.isString json then Set.empty else Set.singleton "Not a string."
