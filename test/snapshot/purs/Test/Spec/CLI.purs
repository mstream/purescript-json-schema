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
import Node.Library.Execa as E
import Test.Snapshot.Utils (Fixture, SnapshotTestSpec)
import Type.Proxy (Proxy(..))

type Input =
  { command ∷ String
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
  [ { input:
        { command: "validate"
        , parameters:
            [ StringNE.nes (Proxy ∷ Proxy "json") /\
                StringNE.nes
                  (Proxy ∷ Proxy "snapshots/json/values/string.json")
            , StringNE.nes (Proxy ∷ Proxy "output-format") /\
                StringNE.nes (Proxy ∷ Proxy "json")
            , StringNE.nes (Proxy ∷ Proxy "schema") /\
                StringNE.nes
                  ( Proxy
                      ∷ Proxy "snapshots/json/schemata/any-number.json"
                  )
            ]
        }
    , outputPath: "json/violations/string-is-not-a-number.json"
    }
  ]

describeInput ∷ Input → String
describeInput { command, parameters } = "a CLI invocation of '"
  <> command
  <> "' with following parameters: "
  <> String.joinWith "," (renderParameter <$> parameters)
  where
  renderParameter ∷ Parameter → String
  renderParameter (k /\ v) =
    StringNE.toString k <> "=" <> StringNE.toString v

executeCommand ∷ Input → Aff String
executeCommand { command, parameters } = do
  cliProcess ← E.execa
    "./cli.sh"
    ( [ command ] <> formatParameters parameters <>
        [ "|", "prettier", "--no-color", "--parser", "json" ]
    )
    identity
  result ← cliProcess.getResult
  case result.exit of
    BySignal _ →
      throwError $ error "process killed"
    Normally 0 →
      pure $ result.stdout <> "\n"
    Normally _ →
      pure $ result.stderr

  where
  formatParameters ∷ Array Parameter → Array String
  formatParameters = foldMap \(k /\ v) →
    [ "--" <> StringNE.toString k, StringNE.toString v ]
