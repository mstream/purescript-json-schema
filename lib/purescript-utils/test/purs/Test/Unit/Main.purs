module Test.Unit.Main (main) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as StringNE
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Test.PMock (Mock, Param, (:>))
import Test.PMock as Mock
import Test.Snapshot.Utils (testSnapshot)
import Test.Spec (Spec)
import Test.Spec (describe, it) as Spec
import Test.Spec.Reporter as SpecReporter
import Test.Spec.Runner as SpecRunner
import Type.Proxy (Proxy(..))

main ∷ Effect Unit
main = Aff.launchAff_
  $ SpecRunner.runSpec [ SpecReporter.consoleReporter ] spec

spec ∷ Spec Unit
spec = Spec.describe "Test.Snapshot.Utils" do
  Spec.it "it calls the init hook only once, despite 2 fixtures" do
    let
      initHookFixture = createSuccessfulInitHookFixture 1
      testSpec = testSnapshot
        { describeInput: const "input"
        , description: StringNE.nes (Proxy @"description")
        , executeCommand: const $ pure "expected output\n"
        , fixtures:
            [ { input: unit
              , outputPath: StringNE.nes (Proxy @"text/dummy")
              }
            , { input: unit
              , outputPath: StringNE.nes (Proxy @"text/dummy")
              }
            ]
        , initHook: initHookFixture.initHook
        }

    initHookFixture.verifyInitHookCalledTimes 0

    resultsAff ← SpecRunner.runSpecT
      SpecRunner.defaultConfig { exit = false }
      []
      testSpec

    void resultsAff

    initHookFixture.verifyInitHookCalledTimes 1

  Spec.it "does not run tests when the init hook fails" do
    let
      initHookFixture = createFailingInitHookFixture 2
      testSpec = testSnapshot
        { describeInput: const "input"
        , description: StringNE.nes (Proxy @"description")
        , executeCommand: const $ pure "expected output\n"
        , fixtures:
            [ { input: unit
              , outputPath: StringNE.nes (Proxy @"text/dummy")
              }
            , { input: unit
              , outputPath: StringNE.nes (Proxy @"text/dummy")
              }
            ]
        , initHook: initHookFixture.initHook
        }

    initHookFixture.verifyInitHookCalledTimes 0

    resultsAff ← SpecRunner.runSpecT
      SpecRunner.defaultConfig { exit = false }
      []
      testSpec
    void resultsAff

    initHookFixture.verifyInitHookCalledTimes 1

type InitHookFixture =
  { initHook ∷ Maybe (Aff Unit)
  , verifyInitHookCalledTimes ∷ Int → Aff Unit
  }

createSuccessfulInitHookFixture ∷ Int → InitHookFixture
createSuccessfulInitHookFixture hookId =
  { initHook: Just do
      Console.info "init hook start"
      pure $ initHookFn hookId
      Console.info "init hook end"
  , verifyInitHookCalledTimes: \expectedTimes →
      Mock.verifyCount initHookFnMock expectedTimes hookId
  }
  where
  initHookFn ∷ Int → Unit
  initHookFn = Mock.fun initHookFnMock

  initHookFnMock ∷ Mock (Int → Unit) (Param Int)
  initHookFnMock = Mock.namedMock
    ("initHookFn_successful_" <> show hookId)
    (Mock.any :> unit)

createFailingInitHookFixture ∷ Int → InitHookFixture
createFailingInitHookFixture hookId =
  { initHook: Just do
      Console.info "init hook start"
      pure $ initHookFn hookId
      throwError $ Exception.error "init hook error"
  , verifyInitHookCalledTimes: \expectedTimes →
      Mock.verifyCount initHookFnMock expectedTimes hookId
  }
  where
  initHookFn ∷ Int → Unit
  initHookFn = Mock.fun initHookFnMock

  initHookFnMock ∷ Mock (Int → Unit) (Param Int)
  initHookFnMock = Mock.namedMock
    ("initHookFn_failing_" <> show hookId)
    (Mock.any :> unit)
