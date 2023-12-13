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
import Node.FS.Constants (x_OK)
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
cliBinPath = "./bin/cli.mjs"

spec ∷ SnapshotTestSpec Input
spec =
  { describeInput
  , description: StringNE.nes (Proxy ∷ Proxy "abc")
  , executeCommand
  , fixtures
  , initHook: Just $ FS.access' cliBinPath x_OK >>= case _ of
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
          ( Proxy
              ∷ Proxy "allowed-types-change-from-number-to-string.json"
          )
      , leftSchemaFile: StringNE.nes (Proxy ∷ Proxy "any-number.json")
      , rightSchemaFile: StringNE.nes (Proxy ∷ Proxy "any-string.json")
      , shouldFail: true
      }
    ]

  diffFixtures ∷ Array (Fixture Input)
  diffFixtures = diffFixture <$>
    [ { differencesFile: StringNE.nes
          ( Proxy
              ∷ Proxy "no-differences.json"
          )
      , leftSchemaFile: StringNE.nes (Proxy ∷ Proxy "any-number.json")
      , rightSchemaFile: StringNE.nes (Proxy ∷ Proxy "any-number.json")
      , shouldFail: false
      }
    , { differencesFile: StringNE.nes
          ( Proxy
              ∷ Proxy "allowed-types-change-from-number-to-string.json"
          )
      , leftSchemaFile: StringNE.nes (Proxy ∷ Proxy "any-number.json")
      , rightSchemaFile: StringNE.nes (Proxy ∷ Proxy "any-string.json")
      , shouldFail: true
      }
    ]

  validateFixtures ∷ Array (Fixture Input)
  validateFixtures = validateFixture <$>
    [ { jsonFile: StringNE.nes (Proxy ∷ Proxy "string.json")
      , schemaFile: StringNE.nes (Proxy ∷ Proxy "any-number.json")
      , shouldFail: true
      , violationsFile: StringNE.nes
          (Proxy ∷ Proxy "string-is-not-a-number.json")
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
      { command: StringNE.nes (Proxy ∷ Proxy "compat")
      , parameters:
          [ StringNE.nes (Proxy ∷ Proxy "left") /\ leftSchemaPath
          , StringNE.nes (Proxy ∷ Proxy "right") /\ rightSchemaPath
          ]
      , shouldFail
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
    , shouldFail ∷ Boolean
    }
  → Fixture Input
diffFixture
  { differencesFile, leftSchemaFile, rightSchemaFile, shouldFail } =
  { input:
      { command: StringNE.nes (Proxy ∷ Proxy "diff")
      , parameters:
          [ StringNE.nes (Proxy ∷ Proxy "left") /\ leftSchemaPath
          , StringNE.nes (Proxy ∷ Proxy "right") /\ rightSchemaPath
          ]
      , shouldFail
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
    , shouldFail ∷ Boolean
    , violationsFile ∷ NonEmptyString
    }
  → Fixture Input
validateFixture { jsonFile, schemaFile, shouldFail, violationsFile } =
  { input:
      { command: StringNE.nes (Proxy ∷ Proxy "validate")
      , parameters:
          [ StringNE.nes (Proxy ∷ Proxy "json") /\ jsonPath
          , StringNE.nes (Proxy ∷ Proxy "schema") /\ schemaPath
          ]
      , shouldFail
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
    cliBinPath
    ( [ StringNE.toString command ] <> formatCliParameters parameters
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
    identity

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
                  "right side of pipeline failed: no  stdin"
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
