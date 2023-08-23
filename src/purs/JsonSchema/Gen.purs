module JsonSchema.Gen
  ( genBooleanSchema
  , genObjectSchema
  , genSchema
  ) where

import Prelude

import Control.Lazy (class Lazy)
import Control.Lazy as Lazy
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenCommon
import Control.Monad.Rec.Class (class MonadRec)
import Data.List (List)
import Data.NonEmpty ((:|))
import Data.Set (Set)
import Data.Set as Set
import Data.String.Gen as StringGen
import JsonSchema (JsonSchema(..), JsonValueType(..))

genSchema
  ∷ ∀ m. Lazy (m JsonSchema) ⇒ MonadGen m ⇒ MonadRec m ⇒ m JsonSchema
genSchema = Lazy.defer \_ → Gen.choose genBooleanSchema genObjectSchema

genBooleanSchema ∷ ∀ m. MonadGen m ⇒ m JsonSchema
genBooleanSchema = BooleanSchema <$> Gen.chooseBool

genObjectSchema
  ∷ ∀ m. Lazy (m JsonSchema) ⇒ MonadGen m ⇒ MonadRec m ⇒ m JsonSchema
genObjectSchema = Lazy.defer \_ →
  ObjectSchema <$>
    do
      items ← GenCommon.genMaybe genSchema
      multipleOf ← GenCommon.genMaybe $ Gen.chooseFloat 0.1 10.0
      not ← GenCommon.genMaybe genSchema
      required ← genSet StringGen.genAlphaString
      typeKeyword ← GenCommon.genMaybe $ genSet genJsonValueType
      uniqueItems ← Gen.chooseBool
      pure
        { items, multipleOf, not, required, typeKeyword, uniqueItems }

genSet ∷ ∀ a m. MonadGen m ⇒ MonadRec m ⇒ Ord a ⇒ m a → m (Set a)
genSet genItem = Set.fromFoldable <$> genItems
  where
  genItems ∷ m (List a)
  genItems = Gen.unfoldable genItem

genJsonValueType ∷ ∀ m. MonadGen m ⇒ m JsonValueType
genJsonValueType = Gen.elements
  $ JsonArray :|
      [ JsonBoolean
      , JsonInteger
      , JsonNull
      , JsonNumber
      , JsonObject
      , JsonString
      ]
