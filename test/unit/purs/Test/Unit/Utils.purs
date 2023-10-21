module Test.Unit.Utils
  ( ComputationTestSpec
  , genValueSample
  , testComputation
  ) where

import Prelude

import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Traversable (traverse_)
import Effect.Class (liftEffect)
import Heterogeneous.Mapping
  ( class MapRecordWithIndex
  , class Mapping
  , ConstMapping
  , hmap
  , mapping
  )
import Prim.RowList (class RowToList)
import Test.QuickCheck as QC
import Test.QuickCheck.Gen (Gen)
import Test.Spec as Spec
import Test.Spec.Assertions as SpecA
import Test.Unit.Computation
  ( ComputationDescription
  , ComputationExample
  , ComputationProperty
  , GetValueDescription(..)
  , GetValueSample(..)
  , ToValueSpec(..)
  , ValueSample(..)
  )
import Test.Unit.TestSpec (class Assertable, TestSpec)

type ComputationTestSpec isa isp o osa r =
  { description ∷ ComputationDescription isp
  , examples ∷ Array (ComputationExample isa osa)
  , execute ∷ { | isa } → o
  , input ∷ { | isp }
  , properties ∷ Array (ComputationProperty isa o)
  | r
  }

testComputation
  ∷ ∀ i isa isp o osa r rlsa
  . Assertable o
  ⇒ Mapping GetValueSample osa o
  ⇒ Mapping GetValueDescription osa NonEmptyString
  ⇒ MapRecordWithIndex rlsa (ConstMapping GetValueSample) isa i
  ⇒ MapRecordWithIndex rlsa (ConstMapping ToValueSpec) isa isp
  ⇒ RowToList isa rlsa
  ⇒ ComputationTestSpec isa isp o osa r
  → TestSpec
testComputation { description, examples, execute, input, properties } =
  Spec.describe (StringNE.toString $ description input) do
    traverse_ testComputationProperty properties
    traverse_ testComputationExample examples
  where
  testComputationExample ∷ ComputationExample isa osa → TestSpec
  testComputationExample { expectedOutput, input: inputSample } =
    Spec.it
      ( "example: "
          <>
            ( StringNE.toString $ description $ hmap ToValueSpec
                inputSample
            )
          <> " should result in "
          <>
            ( StringNE.toString $ mapping GetValueDescription
                expectedOutput
            )
      )
      ( (execute inputSample) `SpecA.shouldEqual`
          (mapping GetValueSample expectedOutput)
      )

  testComputationProperty ∷ ComputationProperty isa o → TestSpec
  testComputationProperty { description: propertyDesc, property } =
    Spec.it ("property: " <> StringNE.toString propertyDesc)
      (liftEffect $ QC.quickCheckGen' 10 (property execute))

genValueSample ∷ ∀ a. NonEmptyString → Gen a → Gen (ValueSample a)
genValueSample description gen = do
  sample ← gen
  pure $ ValueSample { description, sample }
