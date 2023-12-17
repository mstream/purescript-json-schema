module JsonSchema.SchemaPath.Gen (genSchemaPath) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.NonEmpty ((:|))
import Data.String.Gen as StringGen
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import JsonSchema.SchemaPath (SchemaPath, SchemaPathSegment(..))
import Type.Proxy (Proxy(..))

genSchemaPath ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m SchemaPath
genSchemaPath = Gen.unfoldable genSegment
  where
  genSegment ∷ m SchemaPathSegment
  genSegment = Gen.oneOf
    $ pure ExclusiveMaximum :|
        [ pure ExclusiveMinimum
        , pure Items
        , pure Maximum
        , pure MultipleOf
        , Properties <$> genPropertyName
        , pure TypeKeyword
        , pure UniqueItems
        ]

  genPropertyName ∷ m NonEmptyString
  genPropertyName = do
    suffix ← StringGen.genAlphaString
    pure $ StringNE.nes (Proxy @"prop-")
      `StringNE.appendString` suffix
