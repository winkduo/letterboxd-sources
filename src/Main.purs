module Main where

import           Prelude

import           Letterboxd.Html                ( addServices )
import           Effect.Aff                     ( launchAff_ )
import           Effect                         ( Effect )
import           Control.Monad.Except           ( runExceptT )

main :: Effect Unit
main = launchAff_ $ runExceptT addServices
