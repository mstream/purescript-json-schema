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
      not ← GenCommon.genMaybe genSchema
      required ← genSet StringGen.genAlphaString
      typeKeyword ← GenCommon.genMaybe $ genSet genJsonValueType
      uniqueItems ← Gen.chooseBool
      pure { items, not, required, typeKeyword, uniqueItems }

genSet ∷ ∀ a m. MonadGen m ⇒ MonadRec m ⇒ Ord a ⇒ m a → m (Set a)
genSet genItem = Set.fromFoldable <$> genItems
  where
  genItems ∷ m (List a)
  genItems = Gen.unfoldable genItem

{-
genKeyword = Gen.resize (min 3) (Gen.sized go)
  where
  go ∷ Int → m Keyword
  go size = if size == 0 then genLeaf else Gen.resize (_ - 1) genBranch

  genBranch ∷ m Keyword
  genBranch = Gen.oneOf $ genItemsKeyword :| [ genNotKeyword ]

  genLeaf ∷ m Keyword
  genLeaf = Gen.oneOf $ genTypeKeyword :| []
-}

{-
genItemsKeyword
  ∷ ∀ m. Lazy (m JsonSchema) ⇒ MonadGen m ⇒ MonadRec m ⇒ m Keyword
genItemsKeyword = ItemsKeyword <$> genSchema

genNotKeyword
  ∷ ∀ m. Lazy (m JsonSchema) ⇒ MonadGen m ⇒ MonadRec m ⇒ m Keyword
genNotKeyword = NotKeyword <$> genSchema
-}

{-
genTypeKeyword ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m Keyword
genTypeKeyword = (TypeKeyword <<< Set.fromFoldable)
  <$> genJsonValueTypes
  where
  genJsonValueTypes ∷ m (List JsonValueType)
  genJsonValueTypes = Gen.unfoldable genJsonValueType
-}

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
