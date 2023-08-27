module Test.Types
  ( Computation
  , Example
  , ExpectedOutput
  , Input
  , InputGenerator
  , Property
  , TestLength(..)
  , TestSpec
  ) where

import Prelude

import Data.Markdown (Document)
import Effect.Aff (Aff)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (SpecT)

type ExpectedOutput o =
  { description ∷ String
  , value ∷ o
  }

type Example i o =
  { computation ∷ Computation i o
  , description ∷ String
  , expectedOutput ∷ ExpectedOutput o
  , input ∷ Input i
  , renderInput ∷ Input i → Document
  , renderOutput ∷ ExpectedOutput o → Document
  }

type Property i o =
  { computation ∷ Computation i o
  , expectedOutput ∷ ExpectedOutput o
  , input ∷ InputGenerator i
  , renderInput ∷ InputGenerator i → Document
  , renderOutput ∷ ExpectedOutput o → Document
  }

type Input a =
  { description ∷ String
  , value ∷ a
  }

type InputGenerator a =
  { description ∷ String
  , gen ∷ Gen a
  }

type Computation i o =
  { description ∷ String
  , execute ∷ i → o
  }

data TestLength = Long | Medium | Short

type TestSpec = SpecT Aff Unit Aff Unit
