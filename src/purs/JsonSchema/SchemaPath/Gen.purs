module JsonSchema.SchemaPath.Gen (genSchemaPath) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.NonEmpty ((:|))
import Data.String.Gen as StringGen
import JsonSchema.SchemaPath (SchemaPath, SchemaPathSegment(..))

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
        , Properties <$> StringGen.genAlphaString
        , pure TypeKeyword
        , pure UniqueItems
        ]
