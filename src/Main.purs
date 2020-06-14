module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (error)
import Letterboxd.Html (addServices)

main :: Effect Unit
main =
  launchAff_ $ do
    result <- runExceptT addServices
    case result of
      Left err -> error $ show err
      Right _ -> pure unit
