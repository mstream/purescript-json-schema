module JsonSchema.Difference.Gen (genDifference) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenCommon
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe)
import Data.NonEmpty ((:|))
import JsonSchema.Difference (Difference(..), DifferenceType(..))
import JsonSchema.SchemaPath.Gen as SchemaPathGen

genDifference ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ m Difference
genDifference = do
  differenceType ← genType
  path ← SchemaPathGen.genSchemaPath
  pure $ Difference { differenceType, path }
  where
  genType ∷ m DifferenceType
  genType = Gen.oneOf
    $ genBooleanSchemaChange :|
        [ genExclusiveMaximumChange
        ]

genBooleanSchemaChange ∷ ∀ m. MonadGen m ⇒ m DifferenceType
genBooleanSchemaChange = BooleanSchemaChange <$> Gen.chooseBool

genExclusiveMaximumChange ∷ ∀ m. MonadGen m ⇒ m DifferenceType
genExclusiveMaximumChange = ExclusiveMaximumChange
  <$> genMaybeNumber
  <*> genMaybeNumber

genMaybeNumber ∷ ∀ m. MonadGen m ⇒ m (Maybe Number)
genMaybeNumber = GenCommon.genMaybe $ Gen.chooseFloat (-1000.0) 1000.0
