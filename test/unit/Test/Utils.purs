module Test.Utils
  ( TestLength(..)
  , failWithDetails
  , generativeTestCase
  ) where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.Process as Process
import Test.QuickCheck (Result(..), quickCheckGen')
import Test.QuickCheck.Gen (Gen)
import Test.Spec (it)
import Test.Types (TestSpec)

data TestLength = Long | Medium | Short

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
    Medium →
      100
    Long →
      1000

failWithDetails ∷ ∀ r. Show (Record r) ⇒ String → Record r → Result
failWithDetails message details =
  Failed $ message <> "\n" <> show details
