module Test.Snapshot.Spec.Docs (spec) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as ArrayNE
import Data.Markdown as M
import Data.String as String
import Data.String.NonEmpty as StringNE
import Data.Tuple as Tuple
import Docs.Utils (documentComputation)
import Test.QuickCheck (Result(..))
import Test.Snapshot.Utils (Fixture, SnapshotTestSpec)
import Test.Unit.Computation
  ( ComputationSpec
  , ValueSample(..)
  , ValueSpec(..)
  )
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
  , description: StringNE.nes (Proxy ∷ Proxy "abc")
  , executeCommand: pure <<< renderComputationSpec
  , fixtures
  }

fixtures ∷ Array (Fixture Spec)
fixtures =
  [ { input: computation1
    , outputPath: "markdown/computation1.md"
    }
  ]

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
          $ StringNE.nes
              (Proxy ∷ Proxy "computation context")
      ]
  , description: \{ i: ValueSpec i, s: ValueSpec s } →
      StringNE.nes (Proxy ∷ Proxy "computation based on ")
        <> i
        <> StringNE.nes (Proxy ∷ Proxy " and ")
        <> s
  , input:
      { i: ValueSpec
          $
            StringNE.nes (Proxy ∷ Proxy "integers")
      , s: ValueSpec
          $ StringNE.nes (Proxy ∷ Proxy "a string")
      }
  , examples:
      [ { description: StringNE.nes
            (Proxy ∷ Proxy "example1 description")
        , expectedOutput: ValueSample
            { description: StringNE.nes (Proxy ∷ Proxy "false value")
            , sample: false
            }
        , input:
            { i: ValueSample
                { description: StringNE.nes
                    (Proxy ∷ Proxy "number one and two")
                , sample: [ 1, 2 ]
                }
            , s: ValueSample
                { description: StringNE.nes
                    (Proxy ∷ Proxy "a three letter word")
                , sample: "fox"
                }
            }
        }
      , { description: StringNE.nes
            (Proxy ∷ Proxy "example2 description")
        , expectedOutput: ValueSample
            { description: StringNE.nes (Proxy ∷ Proxy "true value")
            , sample: true
            }
        , input:
            { i: ValueSample
                { description: StringNE.nes
                    (Proxy ∷ Proxy "number two, three and four")
                , sample: [ 2, 3, 4 ]
                }
            , s: ValueSample
                { description: StringNE.nes
                    (Proxy ∷ Proxy "a three letter word")
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
      ( Proxy
          ∷ Proxy
              "computation result"
      )
  , properties:
      [ { description: StringNE.nes
            (Proxy ∷ Proxy "property1 description")
        , property: const $ pure Success
        }
      , { description: StringNE.nes
            (Proxy ∷ Proxy "property2 description")
        , property: const $ pure Success
        }
      ]
  }
