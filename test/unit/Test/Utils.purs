module Test.Utils
  ( TestLength(..)
  , generativeTestCase
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldM)
import Data.Maybe (maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.Process as Process
import Test.QuickCheck (Result, quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Test.Spec (it)
import Test.Types (TestSpec)

data TestLength = Long | Short

generativeTestCase ∷ TestLength → String → Gen Result → TestSpec
generativeTestCase testLength title property = do
  shouldRun ← liftEffect $ not <$> checkShouldSkip
  when
    shouldRun
    (it title (liftEffect $ quickCheckGen' iterations property))
  where
  checkShouldSkip ∷ Effect Boolean
  checkShouldSkip = maybe false (_ == "true")
    <$> Process.lookupEnv "SKIP_GENERATIVE_TESTS"

  iterations ∷ Int
  iterations = case testLength of
    Short →
      10
    Long →
      100
