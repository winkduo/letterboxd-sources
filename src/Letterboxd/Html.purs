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
import Data.Argonaut as JSON
import Data.Argonaut.Decode.Generic.Rep as JSON
import Data.Argonaut.Types.Generic.Rep as JSON
import Data.Codec.Argonaut as JSON
import Control.Monad.Except (runExceptT, withExceptT, ExceptT (..), except)
import Effect.Aff.Compat
import Data.Maybe
import Data.Time.Duration (Milliseconds (..))
import Data.Identity
import Data.Generic.Rep (class Generic)
import Data.Nullable
import Data.Tuple

foreign import _addService :: String -> String -> String -> Effect Unit

foreign import _getMovieNameAndYear :: EffectFnAff (Tuple String String)

foreign import _clearServices :: EffectFnAff Unit

foreign import _renderEverything :: String -> MovieStateHtml -> EffectFn1 String Unit -> EffectFn1 String Unit -> Effect Unit

fetch :: M.Fetch
fetch = M.fetch nodeFetch

type Movie = { title :: String, link :: String }

data TransferStatus = IN_QUEUE | WAITING | DOWNLOADING | COMPLETING | SEEDING | COMPLETED | ERROR

data TransferType = TORRENT | URL | PLAYLIST

type Transfer' f = {
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
  , percent_done	:: f Int
  , save_parent_id	:: Maybe Int
  , seconds_seeding	:: Maybe Int
  , size	:: Maybe Number
  , source	:: Maybe String
  , status	:: Maybe String
  , subscription_id	:: Maybe Int
  , tracker_message	:: Maybe String
}

class R2Functor f where
  ffmap :: forall a b. (a ~> b) -> f a -> f b

type Transfer = Transfer' Maybe
type HtmlTransfer = Transfer' Nullable
type FullTransfer = Transfer' Identity

type TransferWithUrl' f = {
    transfer :: Transfer' f
  , url :: f String
}

ffmapTransfer :: forall a b. (a ~> b) -> Transfer' a -> Transfer' b
ffmapTransfer f t = t { percent_done = f t.percent_done }

type TransferWithUrl = TransferWithUrl' Maybe
type FullTransferWithUrl = TransferWithUrl' Identity

baseUrl :: String
baseUrl = "http://localhost:8081"
-- baseUrl = "http://68.183.36.98:8081"

type MovieChannel = {
  id :: String
, link :: String
, title :: String
}

genericJsonEncoding :: JSON.Encoding
genericJsonEncoding =   { tagKey: "tag" , valuesKey: "contents" , unwrapSingleArguments: true }

data MovieChannelState
  = FoundMovieChannel MovieChannel
  | RequestingDownload MovieChannel
  | DownloadingMovie MovieChannel Transfer
  | DownloadedMovie String Transfer
  | CannotDownloadMovie String

type MovieChannelStateHtml =
  { found_movie_channel :: Nullable MovieChannel
  , requesting_download :: Nullable MovieChannel
  , downloading_movie :: Nullable { channel :: MovieChannel, transfer :: HtmlTransfer }
  , downloaded_movie :: Nullable {
      url :: String
    , transfer :: HtmlTransfer }
  , cannot_download_movie :: Nullable String
  }

derive instance genericMovieChannelState :: Generic MovieChannelState _
instance jsonMovieChannelState :: JSON.DecodeJson MovieChannelState where decodeJson = JSON.genericDecodeJsonWith genericJsonEncoding

data MovieState
  = MSSNoChannelsYet
  | MSSUpdatingMovieChannels
  | MSSFoundChannels (Array MovieChannelState)

type MovieStateHtml =
  { no_channels_yet :: Boolean
  , updating_movie_channels :: Boolean
  , found_channels :: Nullable (Array (MovieChannelStateHtml))
  }

derive instance genericMovieState :: Generic MovieState _
instance jsonMovieState :: JSON.DecodeJson MovieState where decodeJson = JSON.genericDecodeJsonWith genericJsonEncoding

startTransfer :: Tuple String String -> ExceptT Exc.Error Aff.Aff (Array TransferWithUrl)
startTransfer (Tuple name year) = do
  log "requesting..."
  response <- ExceptT $ Aff.attempt (fetch (M.URL (baseUrl <> "/find_movie")) { method: M.postMethod, body: "{\"name\": \"" <> name <> "\", \"year\": \"" <> year <> "\"}", headers: M.makeHeaders {"Content-Type": "application/json"} })
  transfersJson <- withExceptT Exc.error $ ExceptT $ JSON.jsonParser <$> M.text response
  withExceptT Exc.error $ except $ JSON.decodeJson transfersJson

sendRequest :: forall resp. JSON.DecodeJson resp => Aff.Aff M.Response -> ExceptT Exc.Error Aff.Aff resp
sendRequest fetchReq = do
  response <- ExceptT $ Aff.attempt fetchReq
  respJson <- withExceptT Exc.error $ ExceptT $ JSON.jsonParser <$> M.text response
  withExceptT Exc.error $ except $ JSON.decodeJson respJson

getMovieState :: Tuple String String -> ExceptT Exc.Error Aff.Aff MovieState
getMovieState (Tuple name year) = do
  sendRequest $ fetch (M.URL (baseUrl <> "/movie_state?name=" <> name <> "&year=" <> year)) { method: M.getMethod, headers: M.makeHeaders {"Content-Type": "application/json"} }

addServices :: ExceptT Exc.Error Aff.Aff (Array TransferWithUrl)
addServices = do
  liftAff $ fromEffectFnAff _clearServices
  movieNameAndYear <- liftAff $ fromEffectFnAff _getMovieNameAndYear
  transfers_with_urls <- startTransfer movieNameAndYear
  log $ show transfers_with_urls
  for_ transfers_with_urls $ \(transfer_with_url) ->
    liftEffect $ _addService transfer_with_url.transfer.name (fromMaybe "#" transfer_with_url.url) (show (fromMaybe 0 transfer_with_url.transfer.percent_done))
  pure transfers_with_urls

toFullTransfer :: Transfer -> FullTransfer
toFullTransfer transfer =
  transfer {
    percent_done = Identity $ fromMaybe 0 transfer.percent_done
  }

toFullTransferWithUrl :: TransferWithUrl -> FullTransferWithUrl
toFullTransferWithUrl transfer_with_url =
  transfer_with_url {
    transfer = toFullTransfer transfer_with_url.transfer,
    url = Identity $ fromMaybe "#" transfer_with_url.url
  }

defaultChannelStateHtml :: MovieChannelStateHtml
defaultChannelStateHtml = {
    found_movie_channel: toNullable Nothing
  , requesting_download: toNullable Nothing
  , downloading_movie: toNullable Nothing
  , downloaded_movie: toNullable Nothing
  , cannot_download_movie: toNullable Nothing
}

translateChannelStateToHtml :: MovieChannelState -> MovieChannelStateHtml
translateChannelStateToHtml (FoundMovieChannel channel) = defaultChannelStateHtml { found_movie_channel = toNullable (Just channel) }
translateChannelStateToHtml (RequestingDownload channel) = defaultChannelStateHtml { requesting_download = toNullable (Just channel) }
translateChannelStateToHtml (DownloadingMovie channel transfer) = defaultChannelStateHtml {
  downloading_movie = toNullable (Just { channel: channel, transfer: ffmapTransfer toNullable transfer })
}
translateChannelStateToHtml (DownloadedMovie url transfer) = defaultChannelStateHtml { downloaded_movie = toNullable (Just { url: url, transfer: ffmapTransfer toNullable transfer }) }
translateChannelStateToHtml (CannotDownloadMovie err) = defaultChannelStateHtml { cannot_download_movie = toNullable (Just err) }

defaultMovieStateHtml :: MovieStateHtml
defaultMovieStateHtml =
  {
    no_channels_yet: false
  , updating_movie_channels: false
  , found_channels: toNullable Nothing
  }

translateMovieStateToHtml :: MovieState -> MovieStateHtml
translateMovieStateToHtml MSSNoChannelsYet = defaultMovieStateHtml { no_channels_yet = true }
translateMovieStateToHtml MSSUpdatingMovieChannels = defaultMovieStateHtml { updating_movie_channels = true }
translateMovieStateToHtml (MSSFoundChannels channels) = defaultMovieStateHtml { found_channels = toNullable (Just (map translateChannelStateToHtml channels)) }

downloadMovieThroughChannel :: String -> String -> String -> Effect Unit
downloadMovieThroughChannel movieName movieYear channelId = do
  Aff.launchAff_ $ do
    result <- runExceptT $ sendRequest $ fetch (M.URL (baseUrl <> "/download_movie?name=" <> movieName <> "&year=" <> movieYear <> "&channel_id=" <> channelId)) { method: M.postMethod, headers: M.makeHeaders {"Content-Type": "application/json"} }
    case result of
         Left err -> log $ show err
         Right (_ :: Unit) -> pure unit

downloadButtonClicked :: String -> String -> EffectFn1 String Unit
downloadButtonClicked movieName movieYear = mkEffectFn1 $ \s -> do
  downloadMovieThroughChannel movieName movieYear s

cancelDownloadClicked :: String -> String -> EffectFn1 String Unit
cancelDownloadClicked movieName movieYear = mkEffectFn1 $ \channelId -> do
  Aff.launchAff_ $ do
    result <- runExceptT $ sendRequest $ fetch (M.URL (baseUrl <> "/cancel_download?name=" <> movieName <> "&year=" <> movieYear <> "&channel_id=" <> channelId)) { method: M.postMethod, headers: M.makeHeaders {"Content-Type": "application/json"} }
    case result of
         Left err -> log $ show err
         Right (_ :: Unit) -> pure unit

renderEverything :: Aff.Aff Unit
renderEverything = do
  Tuple movieName movieYear <- fromEffectFnAff _getMovieNameAndYear
  result <- runExceptT $ getMovieState (Tuple movieName movieYear) 
  
  case result of
       Left err -> log $ show err
       Right movie_state -> do
         liftEffect $ _renderEverything movieName (translateMovieStateToHtml movie_state) (downloadButtonClicked movieName movieYear) (cancelDownloadClicked movieName movieYear)
         Aff.delay (Milliseconds 1000.0)
         renderEverything
