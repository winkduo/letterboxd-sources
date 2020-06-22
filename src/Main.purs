module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Class.Console (error, log)
import Letterboxd.Html (renderEverything)
import Data.Time.Duration
import Control.Monad.Rec.Class


main :: Effect Unit
main = do
  log "wow"
  launchAff_ $ forever $ do
     renderEverything
     delay $ fromDuration $ Seconds (3.0 :: Number)
  {-- result <- runExceptT addServices --}
  {-- case result of --}
  {--   Left err -> --}
  {--     liftEffect $ _renderEverything "Something went wrong..." [] --}
  {--   Right transfers -> --}
  {--     liftEffect $ _renderEverything "Bad Education" transfers --}
