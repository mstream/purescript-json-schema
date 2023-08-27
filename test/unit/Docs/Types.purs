module Docs.Types (Doc, PrintableExample, PrintableProperty) where

import Data.Markdown (Document)
import Data.Set (Set)

type Doc =
  { computationDescription ∷ String
  , examples ∷ Set PrintableExample
  , properties ∷ Set PrintableProperty
  }

type PrintableProperty =
  { title ∷ String
  }

type PrintableExample =
  { description ∷ String
  , input ∷ Document
  , output ∷ Document
  , title ∷ String
  }
