module Main where

import Prelude

import Effect (Effect)
import MoonLander as MoonLander
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI MoonLander.component unit body
