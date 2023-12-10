module Sandbox.Main (main) where

import Prelude

import CLI.Halogen as Halogen
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Sandbox.Component as Component

main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI (Component.component Halogen.commandFormComponent) unit body
