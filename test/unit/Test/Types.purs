module Test.Types (Example, TestLength(..), TestSpec) where

import Prelude

import Effect.Aff (Aff)
import Test.Spec (SpecT)

type Example i o =
  { description ∷ String
  , expectedOutput ∷ o
  , input ∷ i
  , renderInput ∷ i → String
  , renderOutput ∷ o → String
  , title ∷ String
  , transform ∷ i → o
  }

data TestLength = Long | Medium | Short

type TestSpec = SpecT Aff Unit Aff Unit
