module Utils (isInteger) where

import Prelude

import Data.Int as Int

isInteger ∷ Number → Boolean
isInteger x = (Int.toNumber $ Int.trunc x) == x
