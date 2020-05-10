module Letterboxd.Html where

import Prelude
import Effect (Effect)

foreign import _addService :: String -> Effect Unit

addService :: String -> Effect Unit
addService = _addService
