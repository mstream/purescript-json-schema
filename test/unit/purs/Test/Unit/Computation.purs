module Test.Unit.Computation
  ( ComputationContext
  , ComputationDescription
  , ComputationExample
  , ComputationProperty(..)
  , ComputationSpec
  , GetValueDescription(..)
  , GetValueSample(..)
  , ToValueSpec(..)
  , ValueGenerator
  , ValueSample(..)
  , ValueSpec(..)
  ) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Markdown (FlowContentNode)
import Data.Markdown as M
import Data.NonEmpty ((:|))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as StringNE
import Docs.Document (class Document, document)
import Heterogeneous.Mapping (class Mapping)
import Test.QuickCheck (Result)
import Test.QuickCheck.Gen (Gen)

type ComputationSpec isa isp o osa =
  { context ∷ ComputationContext
  , description ∷ ComputationDescription isp
  , input ∷ { | isp }
  , examples ∷ Array (ComputationExample isa osa)
  , execute ∷ { | isa } → o
  , output ∷ ValueSpec o
  , properties ∷ Array (ComputationProperty isa o)
  }

type ComputationContext = Array FlowContentNode
type ComputationDescription isp = { | isp } → NonEmptyString

newtype ValueSpec (a ∷ Type) = ValueSpec NonEmptyString

instance Show (ValueSpec a) where
  show (ValueSpec nes) = StringNE.toString nes

newtype ValueSample (a ∷ Type) = ValueSample
  { description ∷ NonEmptyString, sample ∷ a }

instance (Document a) ⇒ Document (ValueSample a) where
  document (ValueSample { description, sample }) =
    ( M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ StringNE.toString description
            <> ":"
    )
      :| [ M.blockquote $ document sample ]

type ValueGenerator a = { description ∷ String, generator ∷ Gen a }

type ComputationExample (isa ∷ Row Type) (osa ∷ Type) =
  { description ∷ String
  , expectedOutput ∷ osa
  , input ∷ { | isa }
  }

type ComputationProperty (isa ∷ Row Type) (o ∷ Type) =
  { description ∷ String, property ∷ ({ | isa } → o) → Gen Result }

data GetValueDescription = GetValueDescription

instance Mapping GetValueDescription (ValueSample a) NonEmptyString where
  mapping GetValueDescription = \(ValueSample { description }) →
    description

data GetValueSample = GetValueSample

instance Mapping GetValueSample (ValueSample a) a where
  mapping GetValueSample = \(ValueSample { sample }) → sample

data ToValueSpec = ToValueSpec

instance Mapping ToValueSpec (ValueSample a) (ValueSpec a) where
  mapping ToValueSpec = \(ValueSample { description }) → ValueSpec
    description
