module JsonSchema.Codec (parseSchema, printSchema) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as Object
import JsonSchema
  ( JsonArraySchemaSpec
  , JsonIntegerSchemaSpec
  , JsonNumberSchemaSpec
  , JsonObjectSchemaSpec
  , JsonSchema(..)
  , JsonSchemaObjectProperty
  , JsonStringSchemaSpec
  )

parseSchema ∷ Json → String \/ JsonSchema
parseSchema json = do
  schemaObject ← note
    (parsingErrorMessage "schema is not a JSON object")
    (A.toObject json)

  maybe
    (Right JsonEmptySchema)
    ( \schemaTypeJson → case A.toString schemaTypeJson of
        Nothing →
          Left
            $ parsingErrorMessage
            $ "invalid schema type JSON " <> A.stringify schemaTypeJson
        Just "array" →
          JsonArraySchema <$> parseArraySchemaSpec schemaObject
        Just "boolean" →
          Right JsonBooleanSchema
        Just "integer" →
          JsonIntegerSchema <$> parseIntegerSchemaSpec schemaObject
        Just "null" →
          Right JsonNullSchema
        Just "number" →
          JsonNumberSchema <$> parseNumberSchemaSpec schemaObject
        Just "object" →
          JsonObjectSchema <$> parseObjectSchemaSpec schemaObject
        Just "string" →
          JsonStringSchema <$> parseStringSchemaSpec schemaObject
        Just unsupportedType →
          Left
            $ parsingErrorMessage
            $ "unsupported schema type " <> unsupportedType
    )
    (Object.lookup "type" schemaObject)

parseArraySchemaSpec ∷ Object Json → String \/ JsonArraySchemaSpec
parseArraySchemaSpec obj = do
  itemsSchema ← case Object.lookup "items" obj of
    Nothing →
      Right Nothing
    Just itemsJson →
      if A.isNull itemsJson then Right Nothing
      else case parseSchema itemsJson of
        Left errorMessage →
          Left $ parsingErrorMessage errorMessage
        Right schema →
          Right $ Just schema
  uniqueItems ← case Object.lookup "uniqueItems" obj of
    Nothing →
      Right false
    Just uniqueItemsJson →
      case A.toBoolean uniqueItemsJson of
        Nothing →
          Left $ parsingErrorMessage "uniqueItems is not a boolean"
        Just bool →
          Right bool
  Right { itemsSchema, uniqueItems }

parseIntegerSchemaSpec ∷ Object Json → String \/ JsonIntegerSchemaSpec
parseIntegerSchemaSpec obj = do
  Right {}

parseNumberSchemaSpec ∷ Object Json → String \/ JsonNumberSchemaSpec
parseNumberSchemaSpec obj = do
  Right {}

parseObjectSchemaSpec ∷ Object Json → String \/ JsonObjectSchemaSpec
parseObjectSchemaSpec obj = do
  propertySchemaByName ← parsePropertySchemaByName obj
  requiredPropertyNames ← parseRequiredPropertyNames obj

  Right
    { properties:
        ( \name schema →
            { isRequired: name `Set.member` requiredPropertyNames
            , schema
            }
        ) `mapWithIndex` propertySchemaByName
    }
  where
  parsePropertySchemaByName
    ∷ Object Json → String \/ Map String JsonSchema
  parsePropertySchemaByName obj =
    case Object.lookup "properties" obj of
      Nothing →
        Right Map.empty
      Just propertiesJson →
        case A.toObject propertiesJson of
          Just schemaJsonByName →
            Map.fromFoldable <$> traverseWithIndex
              ( \name schemaJson → (name /\ _) <$> parseSchema
                  schemaJson
              )
              schemaJsonByName
          Nothing →
            Left $ parsingErrorMessage "invalid object properties"

  parseRequiredPropertyNames ∷ Object Json → String \/ Set String
  parseRequiredPropertyNames obj = case Object.lookup "required" obj of
    Nothing →
      Right Set.empty
    Just requiredJson →
      case A.toArray requiredJson of
        Just required →
          note "invalid required property name"
            $ Set.fromFoldable <$> traverse A.toString required
        Nothing →
          Left
            $ parsingErrorMessage "invalid required property name list"

parseStringSchemaSpec ∷ Object Json → String \/ JsonNumberSchemaSpec
parseStringSchemaSpec obj = do
  Right {}

parsingErrorMessage ∷ String → String
parsingErrorMessage reason = "Invalid schema: " <> reason

printSchema ∷ JsonSchema → Json
printSchema = case _ of
  JsonArraySchema spec →
    printArraySchema spec
  JsonBooleanSchema →
    printBooleanSchema
  JsonEmptySchema →
    printEmptySchema
  JsonIntegerSchema spec →
    printIntegerSchema spec
  JsonNullSchema →
    printNullSchema
  JsonNumberSchema spec →
    printNumberSchema spec
  JsonObjectSchema spec →
    printObjectSchema spec
  JsonStringSchema spec →
    printStringSchema spec

printArraySchema ∷ JsonArraySchemaSpec → Json
printArraySchema spec = A.fromObject
  $ Object.fromFoldable
      [ "items" /\ case spec.itemsSchema of
          Just itemsSchema →
            printSchema itemsSchema
          Nothing →
            A.jsonNull
      , "type" /\ A.fromString "array"
      , "uniqueItems" /\ A.fromBoolean spec.uniqueItems
      ]

printBooleanSchema ∷ Json
printBooleanSchema = A.fromObject
  $ Object.fromFoldable
      [ "type" /\ A.fromString "boolean"
      ]

printEmptySchema ∷ Json
printEmptySchema = A.jsonEmptyObject

printIntegerSchema ∷ JsonNumberSchemaSpec → Json
printIntegerSchema spec = A.fromObject
  $ Object.fromFoldable
      [ "type" /\ A.fromString "integer"
      ]

printNullSchema ∷ Json
printNullSchema = A.fromObject
  $ Object.fromFoldable
      [ "type" /\ A.fromString "null"
      ]

printNumberSchema ∷ JsonNumberSchemaSpec → Json
printNumberSchema spec = A.fromObject
  $ Object.fromFoldable
      [ "type"
          /\ A.fromString "number"
      ]

printObjectSchema ∷ JsonObjectSchemaSpec → Json
printObjectSchema spec = A.fromObject
  $ Object.fromFoldable
      [ "properties" /\
          ( A.fromObject
              $ Object.fromFoldable
              $ propertySchemaJsonByName spec.properties
          )
      , "required" /\
          ( A.fromArray
              $ Array.fromFoldable
              $ requiredPropertyNameJsons spec.properties
          )
      , "type" /\ A.fromString "object"
      ]
  where
  propertySchemaJsonByName
    ∷ Map String JsonSchemaObjectProperty → List (String /\ Json)
  propertySchemaJsonByName properties =
    Map.toUnfoldable
      $ printSchema <<< _.schema <$> properties

  requiredPropertyNameJsons
    ∷ Map String JsonSchemaObjectProperty → List Json
  requiredPropertyNameJsons = foldlWithIndex
    ( \name acc property →
        if property.isRequired then (A.fromString name) : acc else acc
    )
    Nil

printStringSchema ∷ JsonStringSchemaSpec → Json
printStringSchema spec = A.fromObject
  $ Object.fromFoldable [ "type" /\ A.fromString "string" ]
