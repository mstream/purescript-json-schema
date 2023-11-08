module Test.Snapshot.Utils (Fixture, SnapshotTestSpec, testSnapshot) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.Snapshot.TestSpec (TestSpec)
import Test.Spec as Spec
import Test.Spec.Assertions (fail)

type SnapshotTestSpec i =
  { describeInput ∷ i → String
  , description ∷ NonEmptyString
  , executeCommand ∷ i → Aff String
  , fixtures ∷ Array (Fixture i)
  }

type Fixture i = { input ∷ i, outputPath ∷ String }

testSnapshot ∷ ∀ i. SnapshotTestSpec i → TestSpec
testSnapshot { describeInput, description, executeCommand, fixtures } =
  Spec.describe (StringNE.toString description) do
    traverse_ testFixture fixtures
  where
  testFixture ∷ Fixture i → TestSpec
  testFixture { input, outputPath } =
    let
      title = "snapshot: "
        <> describeInput input
        <> " <=> "
        <> "(("
        <> outputPath
        <> "))"
    in
      Spec.it title do
        expected ← FS.readTextFile UTF8 ("snapshots/" <> outputPath)
        actual ← executeCommand input
        actual `shouldEqualTo` expected

shouldEqualTo ∷ ∀ m. MonadThrow Error m ⇒ String → String → m Unit
shouldEqualTo actual expected = when
  (actual /= expected)
  (fail errorMessage)
  where
  errorMessage ∷ String
  errorMessage = show actual <> "  \n≠\n" <> "  " <> show expected
