module Test.Main where

import Prelude

import Effect (Effect)
import Test.Snapshot.Main as SnapshotMain

main ∷ Effect Unit
main = do
  SnapshotMain.main
