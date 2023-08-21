module Test.Types (Example, TestLength(..), TestSpec) where

import Prelude

import Data.Markdown (Document)
import Effect.Aff (Aff)
import Test.Spec (SpecT)

type Example i o =
  { description ∷ String
  , expectedOutput ∷ o
  , input ∷ i
  , renderInput ∷ i → Document
  , renderOutput ∷ o → Document
  , title ∷ String
  , transform ∷ i → o
  }

data TestLength = Long | Medium | Short

type TestSpec = SpecT Aff Unit Aff Unit
