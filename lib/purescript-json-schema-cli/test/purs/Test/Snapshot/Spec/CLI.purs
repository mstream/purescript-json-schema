module Test.Snapshot.Spec.CLI (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Exception as Exception
import Node.ChildProcess.Types (Exit(..))
import Node.FS.Aff as FS
import Node.FS.Constants (r_OK)
import Node.Library.Execa (ExecaProcess, ExecaResult)
import Node.Library.Execa as E
import Test.Snapshot.Utils (Fixture, SnapshotTestSpec)
import Type.Proxy (Proxy(..))

type Input =
  { command ∷ NonEmptyString
  , parameters ∷ Array Parameter
  , shouldFail ∷ Boolean
  }

type Parameter = NonEmptyString /\ NonEmptyString

cliBinPath ∷ String
cliBinPath = "./dist/node/index.mjs"

snapshotsPath ∷ NonEmptyString → NonEmptyString
snapshotsPath = append $ StringNE.nes (Proxy @"test/snapshots/")

jsonValuesPath ∷ NonEmptyString → NonEmptyString
jsonValuesPath = append
  $ snapshotsPath
  $ StringNE.nes (Proxy @"json/values/")

schemaPath ∷ NonEmptyString → NonEmptyString
schemaPath = append
  $ snapshotsPath
  $ StringNE.nes (Proxy @"json/schemata/")

spec ∷ SnapshotTestSpec Input
spec =
  { describeInput
  , description: StringNE.nes (Proxy @"CLI command execution")
  , executeCommand
  , fixtures
  , initHook: Just $ FS.access' cliBinPath r_OK >>= case _ of
      Just error →
        throwError error
      Nothing →
        pure unit

  }

fixtures ∷ Array (Fixture Input)
fixtures =
  compatFixtures <> diffFixtures <> validateFixtures
  where
  compatFixtures ∷ Array (Fixture Input)
  compatFixtures = compatFixture <$>
    [ { compatibilitiesFile: StringNE.nes
          (Proxy @"allowed-types-change-from-number-to-string.json")
      , leftSchemaFile: StringNE.nes (Proxy @"any-number.json")
      , rightSchemaFile: StringNE.nes (Proxy @"any-string.json")
      , shouldFail: true
      }
    ]

  diffFixtures ∷ Array (Fixture Input)
  diffFixtures = diffFixture <$>
    [ { differencesFile: StringNE.nes
          (Proxy @"no-differences.json")
      , leftSchemaFile: StringNE.nes (Proxy @"any-number.json")
      , rightSchemaFile: StringNE.nes (Proxy @"any-number.json")
      , shouldFail: false
      }
    , { differencesFile: StringNE.nes
          (Proxy @"allowed-types-change-from-number-to-string.json")
      , leftSchemaFile: StringNE.nes (Proxy @"any-number.json")
      , rightSchemaFile: StringNE.nes (Proxy @"any-string.json")
      , shouldFail: true
      }
    ]

  validateFixtures ∷ Array (Fixture Input)
  validateFixtures = validateFixture <$>
    [ { jsonFile: StringNE.nes (Proxy @"string.json")
      , schemaFile: StringNE.nes (Proxy @"any-number.json")
      , shouldFail: true
      , violationsFile: StringNE.nes
          (Proxy @"string-is-not-a-number.json")
      }
    ]

compatFixture
  ∷ { compatibilitiesFile ∷ NonEmptyString
    , leftSchemaFile ∷ NonEmptyString
    , rightSchemaFile ∷ NonEmptyString
    , shouldFail ∷ Boolean
    }
  → Fixture Input
compatFixture
  { compatibilitiesFile, leftSchemaFile, rightSchemaFile, shouldFail } =
  { input:
      { command: StringNE.nes (Proxy @"compat")
      , parameters:
          [ StringNE.nes (Proxy @"left") /\ schemaPath leftSchemaFile
          , StringNE.nes (Proxy @"right") /\ schemaPath rightSchemaFile
          ]
      , shouldFail
      }
  , outputPath: StringNE.nes (Proxy @"json/compatibilities/")
      <> compatibilitiesFile
  }

diffFixture
  ∷ { differencesFile ∷ NonEmptyString
    , leftSchemaFile ∷ NonEmptyString
    , rightSchemaFile ∷ NonEmptyString
    , shouldFail ∷ Boolean
    }
  → Fixture Input
diffFixture
  { differencesFile, leftSchemaFile, rightSchemaFile, shouldFail } =
  { input:
      { command: StringNE.nes (Proxy @"diff")
      , parameters:
          [ StringNE.nes (Proxy @"left") /\ schemaPath leftSchemaFile
          , StringNE.nes (Proxy @"right") /\ schemaPath rightSchemaFile
          ]
      , shouldFail
      }
  , outputPath: StringNE.nes (Proxy @"json/differences/")
      <> differencesFile
  }

validateFixture
  ∷ { jsonFile ∷ NonEmptyString
    , schemaFile ∷ NonEmptyString
    , shouldFail ∷ Boolean
    , violationsFile ∷ NonEmptyString
    }
  → Fixture Input
validateFixture { jsonFile, schemaFile, shouldFail, violationsFile } =
  { input:
      { command: StringNE.nes (Proxy @"validate")
      , parameters:
          [ StringNE.nes (Proxy @"json") /\ jsonValuesPath jsonFile
          , StringNE.nes (Proxy @"schema") /\ schemaPath schemaFile
          ]
      , shouldFail
      }
  , outputPath: StringNE.nes (Proxy @"json/violations/")
      <> violationsFile
  }

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
executeCommand { command, parameters, shouldFail } = do
  result ← runCliProcess `pipe` runPrettierProcess
  let
    { escapedCommand, exit, stderr, stdout } = result
  case exit of
    BySignal _ →
      throwError
        $ Exception.error
        $ "process killed: " <> escapedCommand
    Normally 0 →
      if shouldFail then throwError
        $ Exception.error
        $ "command should fail but it did not: "
            <> show { stderr, stdout }
      else pure $ stdout <> "\n"
    Normally 1 →
      if shouldFail then pure $ stdout <> "\n"
      else throwError
        $ Exception.error
        $ "command should not fail but it did: "
            <> show { exitCode: 1, stderr, stdout }
    Normally exitCode →
      if shouldFail then pure $ stderr <> "\n"
      else throwError
        $ Exception.error
        $ "command should not fail but it did: "
            <> show { exitCode, stderr, stdout }
  where
  runCliProcess ∷ Aff ExecaProcess
  runCliProcess = E.execa
    "node"
    ( [ cliBinPath, StringNE.toString command ]
        <> formatCliParameters parameters
        <>
          [ "--output-format"
          , "json"
          ]
    )
    (_ { shell = Just "bash" })

  runPrettierProcess ∷ Aff ExecaProcess
  runPrettierProcess = E.execa
    "prettier"
    [ "--no-color", "--parser", "json" ]
    (_ { shell = Just "bash" })

formatCliParameters ∷ Array Parameter → Array String
formatCliParameters = foldMap \(k /\ v) →
  [ "--" <> StringNE.toString k, StringNE.toString v ]

pipe ∷ Aff ExecaProcess → Aff ExecaProcess → Aff ExecaResult
pipe runProcess1 runProcess2 = do
  process1 ← runProcess1
  result1 ← process1.getResult
  case result1.exit of
    Normally errorCode →
      if errorCode < 2 then do
        process2 ← runProcess2
        case process2.stdin of
          Nothing →
            throwError
              $ Exception.error
                  "right side of pipeline failed: no stdin"
          Just stdin → do
            stdin.writeUtf8End result1.stdout
            result2 ← process2.getResult
            case result2.exit of
              Normally 0 →
                pure $ result1
                  { exitCode = Just errorCode, stdout = result2.stdout }
              _ →
                throwError
                  $ Exception.error
                  $ "right side of pipeline failed: " <> show result2
      else throwError
        $ Exception.error
        $ "left side of pipeline failed: " <> show result1

    _ →
      throwError
        $ Exception.error
        $ "left side of pipeline failed: " <> show result1
