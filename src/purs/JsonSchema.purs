module JsonSchema
  ( JsonArraySchemaSpec
  , JsonIntegerSchemaSpec
  , JsonNumberSchemaSpec
  , JsonObjectSchemaSpec
  , JsonSchemaObjectProperty
  , JsonSchema(..)
  , JsonStringSchemaSpec
  , ObjectFormJsonSchemaSpec(..)
  , genSchema
  , genArraySchema
  , genIntegerSchema
  , genNumberSchema
  , genObjectSchema
  , genStringSchema
  ) where

import Prelude

import Control.Lazy (class Lazy)
import Control.Lazy as Lazy
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenCommon
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)

data JsonSchema
  = BooleanFormJsonSchema Boolean
  | ObjectFormJsonSchema ObjectFormJsonSchemaSpec

derive instance Generic JsonSchema _
derive instance Eq JsonSchema

instance Show JsonSchema where
  show schema = genericShow schema

data ObjectFormJsonSchemaSpec
  = JsonArraySchema JsonArraySchemaSpec
  | JsonBooleanSchema
  | JsonEmptySchema
  | JsonIntegerSchema JsonIntegerSchemaSpec
  | JsonNullSchema
  | JsonNumberSchema JsonNumberSchemaSpec
  | JsonObjectSchema JsonObjectSchemaSpec
  | JsonStringSchema JsonStringSchemaSpec

derive instance Generic ObjectFormJsonSchemaSpec _
derive instance Eq ObjectFormJsonSchemaSpec

instance Show ObjectFormJsonSchemaSpec where
  show schema = genericShow schema

type JsonArraySchemaSpec =
  { itemsSchema ∷ Maybe JsonSchema
  , uniqueItems ∷ Boolean
  }

type JsonBooleanSchemaSpec = {}

type JsonIntegerSchemaSpec = {}

type JsonNumberSchemaSpec = {}

type JsonStringSchemaSpec = {}

type JsonObjectSchemaSpec =
  { properties ∷ Map String JsonSchemaObjectProperty
  }

type JsonSchemaObjectProperty =
  { isRequired ∷ Boolean
  , schema ∷ JsonSchema
  }

-- genSchema ∷ ∀ m. Lazy (m JsonSchema) ⇒ MonadGen m ⇒ m JsonSchema
-- genSchema = genBooleanFormSchema <|> genObjectFormSchema

genSchema ∷ ∀ m. Lazy (m JsonSchema) ⇒ MonadGen m ⇒ m JsonSchema
genSchema = Lazy.defer \_ →
  Gen.choose genBooleanFormSchema genObjectFormSchema

genBooleanFormSchema
  ∷ ∀ m. Lazy (m JsonSchema) ⇒ MonadGen m ⇒ m JsonSchema
genBooleanFormSchema = Lazy.defer \_ →
  BooleanFormJsonSchema <$> Gen.chooseBool

genObjectFormSchema
  ∷ ∀ m. Lazy (m JsonSchema) ⇒ MonadGen m ⇒ m JsonSchema
genObjectFormSchema = Lazy.defer \_ →
  Gen.oneOf $ genBooleanSchema :|
    [ genArraySchema
    , genEmptySchema
    , genIntegerSchema
    , genNullSchema
    , genNumberSchema
    , genObjectSchema
    , genStringSchema
    ]

genArraySchema ∷ ∀ m. Lazy (m JsonSchema) ⇒ MonadGen m ⇒ m JsonSchema
genArraySchema = do
  itemsSchema ← GenCommon.genMaybe genSchema
  uniqueItems ← Gen.chooseBool
  pure
    $ ObjectFormJsonSchema
    $ JsonArraySchema { itemsSchema, uniqueItems }

genBooleanSchema ∷ ∀ m. MonadGen m ⇒ m JsonSchema
genBooleanSchema = pure $ ObjectFormJsonSchema JsonBooleanSchema

genEmptySchema ∷ ∀ m. MonadGen m ⇒ m JsonSchema
genEmptySchema = pure $ ObjectFormJsonSchema JsonEmptySchema

genIntegerSchema ∷ ∀ m. MonadGen m ⇒ m JsonSchema
genIntegerSchema = pure $ ObjectFormJsonSchema $ JsonIntegerSchema {}

genNullSchema ∷ ∀ m. MonadGen m ⇒ m JsonSchema
genNullSchema = pure $ ObjectFormJsonSchema JsonNullSchema

genNumberSchema ∷ ∀ m. MonadGen m ⇒ m JsonSchema
genNumberSchema = pure $ ObjectFormJsonSchema $ JsonNumberSchema {}

genObjectSchema ∷ ∀ m. Lazy (m JsonSchema) ⇒ MonadGen m ⇒ m JsonSchema
genObjectSchema = do
  properties ← genProperties
  pure $ ObjectFormJsonSchema $ JsonObjectSchema { properties }
  where
  genProperties ∷ m (Map String JsonSchemaObjectProperty)
  genProperties = (Map.fromFoldable <<< List.singleton)
    <$> GenCommon.genTuple (pure "prop") genProperty

  genProperty ∷ m JsonSchemaObjectProperty
  genProperty = do
    isRequired ← Gen.chooseBool
    schema ← genSchema
    pure { isRequired, schema }

genStringSchema ∷ ∀ m. MonadGen m ⇒ m JsonSchema
genStringSchema = pure $ ObjectFormJsonSchema $ JsonStringSchema {}
