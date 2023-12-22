module Test.Snapshot.Utils (Fixture, SnapshotTestSpec, testSnapshot) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe, fromMaybe)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.Spec as Spec
import Test.Spec.Assertions as SpecA
import Test.TestSpec (TestSpec)

type SnapshotTestSpec i =
  { describeInput ∷ i → String
  , description ∷ NonEmptyString
  , executeCommand ∷ i → Aff String
  , fixtures ∷ Array (Fixture i)
  , initHook ∷ Maybe (Aff Unit)
  }

type Fixture i = { input ∷ i, outputPath ∷ NonEmptyString }

testSnapshot ∷ ∀ i. SnapshotTestSpec i → TestSpec
testSnapshot
  { describeInput, description, executeCommand, fixtures, initHook } =
  Spec.beforeAll_ (fromMaybe (pure unit) initHook) do
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
        <> StringNE.toString outputPath
        <> "))"
    in
      Spec.it title do
        expected ← FS.readTextFile UTF8
          ("test/snapshots/" <> StringNE.toString outputPath)
        actual ← executeCommand input
        actual `shouldEqualTo` expected

shouldEqualTo ∷ ∀ m. MonadThrow Error m ⇒ String → String → m Unit
shouldEqualTo actual expected = when
  (actual /= expected)
  (SpecA.fail errorMessage)
  where
  errorMessage ∷ String
  errorMessage = show actual <> "  \n≠\n" <> "  " <> show expected
