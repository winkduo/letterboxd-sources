module Letterboxd.Html where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Traversable (for_)
import Data.Map as Map
import Milkis as M
import Effect.Aff as Aff
import Milkis.Impl.Node (nodeFetch)
import Data.Either
import Effect.Class.Console (log, error)
import Effect.Exception as Exc
import Foreign as Foreign
import Data.Array (take, length)
import Data.Argonaut
import Data.Codec.Argonaut
import Control.Monad.Except (runExceptT, withExceptT, ExceptT (..), except)

foreign import _addService :: String -> String -> Effect Unit

fetch :: M.Fetch
fetch = M.fetch nodeFetch

type Movie = { title :: String, link :: String }

data TransferStatus = IN_QUEUE | WAITING | DOWNLOADING | COMPLETING | SEEDING | COMPLETED | ERROR

data TransferType = TORRENT | URL | PLAYLIST

type Transfer = {
    availability	:: Int
  , created_at	:: String
  , current_ratio	:: Int
  , downloaded	:: Int
  , uploaded	:: Int
  , down_speed	:: Int
  , up_speed	:: Int
  , error_message	:: String
  , estimated_time	:: Int
  , file_id	:: Int
  , finished_at	:: String
  , id	:: Int
  , is_private	:: Boolean
  , name	:: String
  , peers	:: Int
  , percent_done	:: Int
  , save_parent_id	:: Int
  , seconds_seeding	:: Int
  , size	:: Int
  , source	:: String
  , status	:: String
  , subscription_id	:: Int
  , tracker_message	:: String
}

findMovies :: ExceptT Exc.Error Aff.Aff (Array Movie)
findMovies = do
  response <- ExceptT $ Aff.attempt (fetch (M.URL "http://localhost:8080/find_movie?name=Millennium") { method: M.getMethod })
  let code = M.statusCode response
  moviesJson <- withExceptT Exc.error $ ExceptT $ jsonParser <$> M.text response
  withExceptT Exc.error $ except $ decodeJson moviesJson

startTransfer :: String -> ExceptT Exc.Error Aff.Aff Transfer
startTransfer magnet_url = do
  response <- ExceptT $ Aff.attempt (fetch (M.URL ("http://localhost:8080/download_movie")) { method: M.postMethod, body: "{\"magnet_url\": \"" <> magnet_url <> "\"}", headers: M.makeHeaders {"Content-Type": "application/json"} })
  let code = M.statusCode response
  transfer <- withExceptT Exc.error $ ExceptT $ jsonParser <$> M.text response
  withExceptT Exc.error $ except $ decodeJson transfer

addServices :: ExceptT Exc.Error Aff.Aff Unit
addServices = do
  movies <- findMovies
  for_ (take 5 movies) $ \(movie :: Movie) -> do
    log $ show movie
    transfer <- startTransfer movie.link
    log $ show transfer
    liftEffect $ _addService movie.title movie.link
