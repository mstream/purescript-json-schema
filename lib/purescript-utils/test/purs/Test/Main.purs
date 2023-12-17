module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main as UnitMain

main ∷ Effect Unit
main = do
  UnitMain.main
