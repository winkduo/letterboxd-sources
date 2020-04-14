module Chrome.Utils where

import Prelude

import Effect (Effect)
import Data.Time.Duration (Milliseconds(..))

foreign import _now :: Effect Number
foreign import _addService :: Effect Unit

now :: Effect Milliseconds
now = Milliseconds <$> _now

addService :: Effect Unit
addService = _addService
