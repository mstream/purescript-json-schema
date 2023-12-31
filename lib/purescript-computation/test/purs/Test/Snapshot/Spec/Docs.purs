module Test.Snapshot.Spec.Docs (spec) where

import Prelude

import Computation (ComputationSpec, ValueSample(..), ValueSpec(..))
import Computation.Utils (documentComputation)
import Data.Array as Array
import Data.Array.NonEmpty as ArrayNE
import Data.Markdown as M
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Data.Tuple as Tuple
import Show.NonEmpty (show1)
import Test.QuickCheck (Result(..))
import Test.Snapshot.Utils (Fixture, SnapshotTestSpec)
import Type.Proxy (Proxy(..))

type Spec = ComputationSpec InputSample InputSpec Output OutputSample

type Input = (i ∷ Array Int, s ∷ String)

type InputSpec = (i ∷ ValueSpec (Array Int), s ∷ ValueSpec String)

type InputSample = (i ∷ ValueSample (Array Int), s ∷ ValueSample String)

type Output = Boolean
type OutputSample = ValueSample Output

spec ∷ SnapshotTestSpec Spec
spec =
  { describeInput
  , description: StringNE.nes (Proxy @"rendering computation spec")
  , executeCommand: pure <<< renderComputationSpec
  , fixtures
  , initHook: Nothing
  }

fixtures ∷ Array (Fixture Spec)
fixtures =
  renderFixture <$>
    [ { computationSpec: computation1
      , outputFile: StringNE.nes (Proxy @"computation1.md")
      }
    ]

renderFixture
  ∷ { computationSpec ∷ Spec, outputFile ∷ NonEmptyString }
  → Fixture Spec
renderFixture { computationSpec, outputFile } =
  { input: computationSpec, outputPath }
  where
  outputPath ∷ NonEmptyString
  outputPath = StringNE.nes (Proxy @"markdown/") <> outputFile

describeInput ∷ Spec → String
describeInput { input, output } = "a computation with input of '"
  <> show input
  <> "' and output of '"
  <> show output
  <> "'"

renderComputationSpec ∷ Spec → String
renderComputationSpec = M.render { maxLineLength: 72 }
  <<< Tuple.snd
  <<< documentComputation

computation1 ∷ Spec
computation1 =
  { context:
      [ M.paragraph
          $ ArrayNE.singleton
          $ M.text
          $ StringNE.nes (Proxy @"computation context")
      ]
  , description: \oSpec { i: iSpec, s: sSpec } →
      StringNE.nes (Proxy @"computation of ")
        <> show1 oSpec
        <> StringNE.nes (Proxy @" based on ")
        <> show1 iSpec
        <> StringNE.nes (Proxy @" and ")
        <> show1 sSpec
  , input:
      { i: ValueSpec
          $
            StringNE.nes (Proxy @"integers")
      , s: ValueSpec
          $ StringNE.nes (Proxy @"a string")
      }
  , examples:
      [ { description: StringNE.nes
            (Proxy @"example1 description")
        , expectedOutput: ValueSample
            { description: StringNE.nes (Proxy @"false value")
            , sample: false
            }
        , input:
            { i: ValueSample
                { description: StringNE.nes
                    (Proxy @"number one and two")
                , sample: [ 1, 2 ]
                }
            , s: ValueSample
                { description: StringNE.nes
                    (Proxy @"a three letter word")
                , sample: "fox"
                }
            }
        }
      , { description: StringNE.nes
            (Proxy @"example2 description")
        , expectedOutput: ValueSample
            { description: StringNE.nes (Proxy @"true value")
            , sample: true
            }
        , input:
            { i: ValueSample
                { description: StringNE.nes
                    (Proxy @"number two, three and four")
                , sample: [ 2, 3, 4 ]
                }
            , s: ValueSample
                { description: StringNE.nes
                    (Proxy @"a three letter word")
                , sample: "fox"
                }
            }
        }
      ]
  , execute:
      \{ i: ValueSample { sample: i }
       , s: ValueSample { sample: s }
       } →
        String.length s == Array.length i
  , output: ValueSpec $ StringNE.nes
      (Proxy @"some result")
  , properties:
      [ { description: StringNE.nes
            (Proxy @"property1 description")
        , property: const $ pure Success
        }
      , { description: StringNE.nes
            (Proxy @"property2 description")
        , property: const $ pure Success
        }
      ]
  }
