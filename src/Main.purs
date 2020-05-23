module Main where

import Prelude

import Letterboxd.Html (addServices)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect (Effect)

main :: Effect Unit
main = launchAff_ do
  liftEffect addServices
