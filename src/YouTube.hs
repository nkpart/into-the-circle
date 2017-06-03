{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module YouTube where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Foldable          (traverse_)
import           Data.Text              (Text)
import           Network.Wreq           (param)
import qualified Network.Wreq           as Wreq
import qualified Pipes
import qualified Pipes.Prelude          as Pipes
import           Types


listVideosForUser :: Text -> Query -> IO [Video]
listVideosForUser apiKey user = do
  Just thing <- getChannelUploadsId apiKey user
  let xs = Pipes.for (readAllPages apiKey thing Nothing) (traverse_ Pipes.yield)
  fmap (fmap (setSource user)) (Pipes.toListM xs)

setSource q v = v { _videoSource = q }

readAllPages
  :: Pipes.MonadIO m
  => Text -> Text -> Maybe Text -> Pipes.Producer [UnsourcedVideo] m ()
readAllPages apiKey playlist pageToken = do
  do (token, items) <- liftIO (getChannelUploads apiKey playlist pageToken)
     Pipes.yield items
     case token of
       Just v  -> readAllPages apiKey playlist (Just v)
       Nothing -> pure ()

instance FromJSON UnsourcedVideo where
  parseJSON =
    withObject "Video" $ \o -> do
      snippet <- o .: "snippet"
      contentDetails <- o .: "contentDetails"
      Video <$> (contentDetails .: "videoId") <*> (snippet .: "title") <*>
        (snippet .: "description") <*>
        (snippet .: "publishedAt") <*> pure ()

instance ToJSON UnsourcedVideo where
  toJSON = undefined

getChannelUploadsId :: Text -> Query -> IO (Maybe Text)
getChannelUploadsId apiKey forUsername = do
  let opts =
        Wreq.defaults & param "part" .~ ["contentDetails"] & pp & param "key" .~
        [apiKey]
      pp =
        case forUsername of
          Username u    -> param "forUsername" .~ [u]
          ChannelId c _ -> param "id" .~ [c]
  r <- Wreq.getWith opts "https://www.googleapis.com/youtube/v3/channels"
  pure $ r ^? Wreq.responseBody . key "items" . nth 0 . key "contentDetails" .
    key "relatedPlaylists" .
    key "uploads" .
    _String

getChannelUploads :: Text -> Text -> Maybe Text -> IO (Maybe Text, [UnsourcedVideo])
getChannelUploads apiKey playlistId pageToken = do
  let opts =
        Wreq.defaults & param "part" .~ ["snippet,contentDetails"] &
        param "maxResults" .~
        ["50"] &
        param "key" .~
        [apiKey] &
        param "playlistId" .~
        [playlistId]
      opts' = maybe id (\v -> param "pageToken" .~ [v]) pageToken opts
  r <- Wreq.getWith opts' "https://www.googleapis.com/youtube/v3/playlistItems"
  let jsonBody = r ^?! Wreq.responseBody
  let itemPart = jsonBody ^.. key "items" . _Array . traverse . _JSON
      tokenPart = jsonBody ^? key "nextPageToken" . _String
  pure (tokenPart, itemPart)
