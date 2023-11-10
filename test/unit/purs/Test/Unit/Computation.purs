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
import Show.NonEmpty (class Show1)
import Test.QuickCheck (Result)
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy(..))

type ComputationSpec isa isp o osa =
  { context ∷ ComputationContext
  , description ∷ ComputationDescription isp o
  , input ∷ { | isp }
  , examples ∷ Array (ComputationExample isa osa)
  , execute ∷ { | isa } → o
  , output ∷ ValueSpec o
  , properties ∷ Array (ComputationProperty isa o)
  }

type ComputationContext = Array FlowContentNode

type ComputationDescription isp o =
  ValueSpec o → { | isp } → NonEmptyString

newtype ValueSpec (a ∷ Type) = ValueSpec NonEmptyString

instance Show (ValueSpec a) where
  show (ValueSpec nes) = StringNE.toString nes

instance Show1 (ValueSpec a) where
  show1 (ValueSpec nes) = nes

newtype ValueSample (a ∷ Type) = ValueSample
  { description ∷ NonEmptyString, sample ∷ a }

derive newtype instance Show a ⇒ Show (ValueSample a)

instance (Document a) ⇒ Document (ValueSample a) where
  document (ValueSample { description, sample }) =
    ( M.paragraph
        $ ArrayNE.singleton
        $ M.text
        $ description <> StringNE.nes (Proxy ∷ Proxy ":")
    )
      :| [ M.blockquote $ document sample ]

type ValueGenerator a = { description ∷ String, generator ∷ Gen a }

type ComputationExample (isa ∷ Row Type) (osa ∷ Type) =
  { description ∷ NonEmptyString
  , expectedOutput ∷ osa
  , input ∷ { | isa }
  }

type ComputationProperty (isa ∷ Row Type) (o ∷ Type) =
  { description ∷ NonEmptyString
  , property ∷ ({ | isa } → o) → Gen Result
  }

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
