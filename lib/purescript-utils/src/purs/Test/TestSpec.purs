module Test.TestSpec (class Assertable, TestSpec, runAll) where

import Prelude

import Data.Foldable (class Foldable, sequence_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Test.Spec (SpecT)
import Test.Spec.Reporter as SpecReporter
import Test.Spec.Runner as SpecRunner

class Assertable ∷ Type → Constraint
class (Eq a, Show a) ⇐ Assertable a

instance (Eq a, Show a) ⇒ Assertable a

type TestSpec = SpecT Aff Unit Aff Unit

runAll ∷ ∀ f. Foldable f ⇒ f TestSpec → Aff Unit
runAll specs = do
  resultsAff ← SpecRunner.runSpecT
    SpecRunner.defaultConfig
      { slow = Milliseconds $ timeoutInMillis / 10.0
      , timeout = Just $ Milliseconds timeoutInMillis
      }
    [ SpecReporter.consoleReporter ]
    (sequence_ specs)

  void resultsAff
  where
  timeoutInMillis ∷ Number
  timeoutInMillis = 10000.0
