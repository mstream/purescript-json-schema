module Test.Snapshot.Spec.CLI (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Foldable (foldMap)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, error)
import Node.ChildProcess.Types (Exit(..))
import Node.Library.Execa (ExecaProcess, ExecaResult)
import Node.Library.Execa as E
import Test.Snapshot.Utils (Fixture, SnapshotTestSpec)
import Type.Proxy (Proxy(..))

type Input =
  { command ∷ NonEmptyString
  , parameters ∷ Array Parameter
  }

type Parameter = NonEmptyString /\ NonEmptyString

spec ∷ SnapshotTestSpec Input
spec =
  { describeInput
  , description: StringNE.nes (Proxy ∷ Proxy "abc")
  , executeCommand
  , fixtures
  }

fixtures ∷ Array (Fixture Input)
fixtures =
  compatFixtures <> diffFixtures <> validateFixtures
  where
  compatFixtures ∷ Array (Fixture Input)
  compatFixtures = compatFixture <$>
    [ { compatibilitiesFile: StringNE.nes
          ( Proxy
              ∷ Proxy "allowed-types-change-from-number-to-string.json"
          )
      , leftSchemaFile: StringNE.nes (Proxy ∷ Proxy "any-number.json")
      , rightSchemaFile: StringNE.nes (Proxy ∷ Proxy "any-string.json")
      }
    ]

  diffFixtures ∷ Array (Fixture Input)
  diffFixtures = diffFixture <$>
    [ { differencesFile: StringNE.nes
          ( Proxy
              ∷ Proxy "allowed-types-change-from-number-to-string.json"
          )
      , leftSchemaFile: StringNE.nes (Proxy ∷ Proxy "any-number.json")
      , rightSchemaFile: StringNE.nes (Proxy ∷ Proxy "any-string.json")
      }
    ]

  validateFixtures ∷ Array (Fixture Input)
  validateFixtures = validateFixture <$>
    [ { jsonFile: StringNE.nes (Proxy ∷ Proxy "string.json")
      , schemaFile: StringNE.nes (Proxy ∷ Proxy "any-number.json")
      , violationsFile: StringNE.nes
          (Proxy ∷ Proxy "string-is-not-a-number.json")
      }
    ]

compatFixture
  ∷ { compatibilitiesFile ∷ NonEmptyString
    , leftSchemaFile ∷ NonEmptyString
    , rightSchemaFile ∷ NonEmptyString
    }
  → Fixture Input
compatFixture { compatibilitiesFile, leftSchemaFile, rightSchemaFile } =
  { input:
      { command: StringNE.nes (Proxy ∷ Proxy "compat")
      , parameters:
          [ StringNE.nes (Proxy ∷ Proxy "left") /\ leftSchemaPath
          , StringNE.nes (Proxy ∷ Proxy "right") /\ rightSchemaPath
          ]
      }
  , outputPath
  }
  where
  leftSchemaPath ∷ NonEmptyString
  leftSchemaPath =
    StringNE.nes (Proxy ∷ Proxy "snapshots/json/schemata/")
      <> leftSchemaFile

  rightSchemaPath ∷ NonEmptyString
  rightSchemaPath =
    StringNE.nes (Proxy ∷ Proxy "snapshots/json/schemata/")
      <> rightSchemaFile

  outputPath ∷ NonEmptyString
  outputPath = StringNE.nes (Proxy ∷ Proxy "json/compatibilities/")
    <> compatibilitiesFile

diffFixture
  ∷ { differencesFile ∷ NonEmptyString
    , leftSchemaFile ∷ NonEmptyString
    , rightSchemaFile ∷ NonEmptyString
    }
  → Fixture Input
diffFixture { differencesFile, leftSchemaFile, rightSchemaFile } =
  { input:
      { command: StringNE.nes (Proxy ∷ Proxy "diff")
      , parameters:
          [ StringNE.nes (Proxy ∷ Proxy "left") /\ leftSchemaPath
          , StringNE.nes (Proxy ∷ Proxy "right") /\ rightSchemaPath
          ]
      }
  , outputPath
  }
  where
  leftSchemaPath ∷ NonEmptyString
  leftSchemaPath =
    StringNE.nes (Proxy ∷ Proxy "snapshots/json/schemata/")
      <> leftSchemaFile

  rightSchemaPath ∷ NonEmptyString
  rightSchemaPath =
    StringNE.nes (Proxy ∷ Proxy "snapshots/json/schemata/")
      <> rightSchemaFile

  outputPath ∷ NonEmptyString
  outputPath = StringNE.nes (Proxy ∷ Proxy "json/differences/")
    <> differencesFile

validateFixture
  ∷ { jsonFile ∷ NonEmptyString
    , schemaFile ∷ NonEmptyString
    , violationsFile ∷ NonEmptyString
    }
  → Fixture Input
validateFixture { jsonFile, schemaFile, violationsFile } =
  { input:
      { command: StringNE.nes (Proxy ∷ Proxy "validate")
      , parameters:
          [ StringNE.nes (Proxy ∷ Proxy "json") /\ jsonPath
          , StringNE.nes (Proxy ∷ Proxy "schema") /\ schemaPath
          ]
      }
  , outputPath
  }
  where
  jsonPath ∷ NonEmptyString
  jsonPath = StringNE.nes (Proxy ∷ Proxy "snapshots/json/values/")
    <> jsonFile

  schemaPath ∷ NonEmptyString
  schemaPath = StringNE.nes (Proxy ∷ Proxy "snapshots/json/schemata/")
    <> schemaFile

  outputPath ∷ NonEmptyString
  outputPath = StringNE.nes (Proxy ∷ Proxy "json/violations/")
    <> violationsFile

describeInput ∷ Input → String
describeInput { command, parameters } = "a CLI invocation of '"
  <> StringNE.toString command
  <> "' with following parameters: "
  <> String.joinWith "," (renderParameter <$> parameters)
  where
  renderParameter ∷ Parameter → String
  renderParameter (k /\ v) =
    StringNE.toString k <> "=" <> StringNE.toString v

executeCommand ∷ Input → Aff String
executeCommand { command, parameters } = do
  result ← runCliProcess `pipe` runPrettierProcess
  case result.exit of
    BySignal _ →
      throwError $ error $ "process killed: " <> result.escapedCommand
    Normally 0 → do
      pure $ result.stdout <> "\n"
    Normally _ →
      pure $ result.stderr <> "\n"
  where
  runCliProcess ∷ Aff ExecaProcess
  runCliProcess = E.execa
    "./cli.sh"
    ( [ StringNE.toString command ] <> formatCliParameters parameters
        <>
          [ "--output-format"
          , "json"
          ]
    )
    identity

  runPrettierProcess ∷ Aff ExecaProcess
  runPrettierProcess = E.execa
    "prettier"
    [ "--no-color", "--parser", "json" ]
    identity

formatCliParameters ∷ Array Parameter → Array String
formatCliParameters = foldMap \(k /\ v) →
  [ "--" <> StringNE.toString k, StringNE.toString v ]

pipe ∷ Aff ExecaProcess → Aff ExecaProcess → Aff ExecaResult
pipe runProcess1 runProcess2 = do
  process1 ← runProcess1
  result1 ← process1.getResult
  case result1.exit of
    Normally 0 → do
      process2 ← runProcess2
      process2.stdin.writeUtf8End result1.stdout
      process2.getResult
    _ →
      pure result1
