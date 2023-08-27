module Test.Utils
  ( exampleTestCase
  , exampleTitle
  , failWithDetails
  , generativeTestCase
  , propertyTest
  , propertyTitle
  ) where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.Process as Process
import Test.QuickCheck (Result(..), quickCheckGen', (===))
import Test.QuickCheck.Gen (Gen)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Types (Example, Property, TestLength(..), TestSpec)

propertyTest
  ∷ ∀ i o. Eq o ⇒ Show o ⇒ TestLength → Property i o → TestSpec
propertyTest testLength property =
  generativeTestCase testLength (propertyTitle property) genResult
  where
  genResult ∷ Gen Result
  genResult = do
    input ← property.input.gen

    let
      actualOutputValue = property.computation.execute input

    pure $ actualOutputValue === property.expectedOutput.value

propertyTitle ∷ ∀ i o. Property i o → String
propertyTitle { computation, expectedOutput, input } =
  computation.description
    <> " "
    <> input.description
    <> " should give "
    <> expectedOutput.description

generativeTestCase ∷ TestLength → String → Gen Result → TestSpec
generativeTestCase testLength title genResult = do
  shouldRun ← liftEffect areGenerativeTestsEnabled
  when
    shouldRun
    ( it
        ("Property: " <> title)
        (liftEffect $ quickCheckGen' (iterations testLength) genResult)
    )

exampleTestCase ∷ ∀ i o. Eq o ⇒ Show o ⇒ Example i o → TestSpec
exampleTestCase example = it ("Example: " <> exampleTitle example) do
  actual `shouldEqual` example.expectedOutput.value
  where
  actual ∷ o
  actual = example.computation.execute example.input.value

exampleTitle ∷ ∀ i o. Example i o → String
exampleTitle { input } = input.description

failWithDetails ∷ ∀ r. Show (Record r) ⇒ String → Record r → Result
failWithDetails message details =
  Failed $ message <> "\n" <> show details

areGenerativeTestsEnabled ∷ Effect Boolean
areGenerativeTestsEnabled = not <$> areDisabled
  where
  areDisabled ∷ Effect Boolean
  areDisabled = maybe false (_ == "true")
    <$> Process.lookupEnv "SKIP_GENERATIVE_TESTS"

iterations ∷ TestLength → Int
iterations = case _ of
  Short →
    10
  Medium →
    100
  Long →
    1000
