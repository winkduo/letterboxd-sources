module Letterboxd.Html where

import Prelude
import Effect (Effect)
import Data.Traversable (for_)
import Data.Map as Map
import Data.List.Lazy.Types as List
import Milkis as M
import Effect.Aff as Aff
import Milkis.Impl.Node (nodeFetch)
import Data.Either
import Effect.Class.Console (log)
import Effect.Class (liftEffect)
import Control.Monad.Except (runExcept)
import Foreign as Foreign
import Data.Array (take, length)
import Data.Argonaut

foreign import _addService :: String -> String -> Effect Unit

fetch :: M.Fetch
fetch = M.fetch nodeFetch

type Movie = { _mTitle :: String, _mLink :: String }

addServices :: Effect Unit
addServices = Aff.launchAff_ do
  _response <- Aff.attempt $ fetch (M.URL "http://localhost:8080/find_movie?name=Millennium") { method: M.getMethod }
  case _response of
    Left e ->
      liftEffect $ log ("failed with " <> show e)
    Right response -> do
      let code = M.statusCode response
      e_movies_json <- jsonParser <$> M.text response
      case e_movies_json of
           Left parseErr ->
             liftEffect $ log "Could not parse response."
           Right moviesJson ->
              case decodeJson moviesJson of
                   Left parseErr2 -> 
                     liftEffect $ log "Could not parse response 2."
                   Right movies ->
                     for_ (take 5 movies) $ \(movie :: Movie) ->
                       liftEffect $ _addService movie._mTitle movie._mLink
