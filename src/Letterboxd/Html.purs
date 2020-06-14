module Letterboxd.Html where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Traversable (for_)
import Data.Map as Map
import Milkis as M
import Effect.Aff as Aff
import Effect.Aff.Class
import Milkis.Impl.Node (nodeFetch)
import Data.Either
import Effect.Class.Console (log, error)
import Effect.Exception as Exc
import Foreign as Foreign
import Data.Array (take, length)
import Data.Argonaut
import Data.Codec.Argonaut
import Control.Monad.Except (runExceptT, withExceptT, ExceptT (..), except)
import Effect.Aff.Compat
import Data.Maybe

foreign import _addService :: String -> String -> String -> Effect Unit

foreign import _getMovieName :: EffectFnAff String

foreign import _clearServices :: EffectFnAff Unit

fetch :: M.Fetch
fetch = M.fetch nodeFetch

type Movie = { title :: String, link :: String }

data TransferStatus = IN_QUEUE | WAITING | DOWNLOADING | COMPLETING | SEEDING | COMPLETED | ERROR

data TransferType = TORRENT | URL | PLAYLIST

type Transfer = {
    availability	:: Maybe Int
  , created_at	:: Maybe String
  , current_ratio	:: Maybe Number
  , downloaded	:: Maybe Number
  , uploaded	:: Maybe Number
  , down_speed	:: Maybe Number
  , up_speed	:: Maybe Number
  , error_message	:: Maybe String
  , estimated_time	:: Maybe Int
  , file_id	:: Maybe Int
  , finished_at	:: Maybe String
  , id	:: Maybe Int
  , is_private	:: Maybe Boolean
  , name	:: String
  , peers	:: Maybe Int
  , percent_done	:: Maybe Int
  , save_parent_id	:: Maybe Int
  , seconds_seeding	:: Maybe Int
  , size	:: Maybe Number
  , source	:: Maybe String
  , status	:: Maybe String
  , subscription_id	:: Maybe Int
  , tracker_message	:: Maybe String
}

type TransferWithUrl = {
    transfer :: Transfer
  , url :: Maybe String
}

baseUrl :: String
-- baseUrl = "http://localhost:8080"
baseUrl = "http://68.183.36.98:8081"

findMovies :: ExceptT Exc.Error Aff.Aff (Array Movie)
findMovies = do
  response <- ExceptT $ Aff.attempt (fetch (M.URL (baseUrl <> "/find_movie?name=Millennium")) { method: M.getMethod })
  let code = M.statusCode response
  moviesJson <- withExceptT Exc.error $ ExceptT $ jsonParser <$> M.text response
  withExceptT Exc.error $ except $ decodeJson moviesJson

startTransfer :: String -> ExceptT Exc.Error Aff.Aff (Array TransferWithUrl)
startTransfer name = do
  log "requesting..."
  response <- ExceptT $ Aff.attempt (fetch (M.URL (baseUrl <> "/find_movie")) { method: M.postMethod, body: "{\"name\": \"" <> name <> "\"}", headers: M.makeHeaders {"Content-Type": "application/json"} })
  transfersJson <- withExceptT Exc.error $ ExceptT $ jsonParser <$> M.text response
  withExceptT Exc.error $ except $ decodeJson transfersJson

addServices :: ExceptT Exc.Error Aff.Aff Unit
addServices = do
  liftAff $ fromEffectFnAff _clearServices
  movieName <- liftAff $ fromEffectFnAff _getMovieName
  transfers_with_urls <- startTransfer movieName
  log $ show transfers_with_urls
  for_ transfers_with_urls $ \(transfer_with_url) ->
    liftEffect $ _addService transfer_with_url.transfer.name (fromMaybe "#" transfer_with_url.url) (show (fromMaybe 0 transfer_with_url.transfer.percent_done))
  pure unit
