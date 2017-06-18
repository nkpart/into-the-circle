{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module YouTube where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Foldable          (traverse_)
import           Data.Text              (Text, intercalate)
import           Network.Wreq           (param)
import qualified Network.Wreq           as Wreq
import qualified Pipes
import qualified Pipes.Prelude          as Pipes
import           Types

listVideosForUser :: YoutubeApiKey -> Query -> IO [Video]
listVideosForUser apiKey user = do
  Just thing <- getChannelUploadsId apiKey user
  let xs = Pipes.for (readAllPages apiKey thing Nothing) (traverse_ Pipes.yield)
  Pipes.toListM xs

readAllPages
  :: Pipes.MonadIO m
  => YoutubeApiKey -> Text -> Maybe Text -> Pipes.Producer [Video] m ()
readAllPages apiKey playlist pageToken = do
  do (token, items) <- liftIO (getChannelUploads apiKey playlist pageToken)
     deets <- liftIO $ getVideosDetails apiKey (fmap _videoId items)
     if (length items == length deets)
       then Pipes.yield (zipWith (set videoExt) deets items)
       else fail "did not expect this"

     case token of
       Just v  -> readAllPages apiKey playlist (Just v)
       Nothing -> pure ()

instance FromJSON VideoU where
  parseJSON =
    withObject "Video" $ \o -> do
        snippet <- o .: "snippet"
        contentDetails <- o .: "contentDetails"
        Video <$> (VideoId <$> contentDetails .: "videoId") <*>
          (snippet .: "title") <*>
          (snippet .: "description") <*>
          (snippet .: "publishedAt") <*>
          (Channel <$> snippet .: "channelTitle") <*>
          pure ()

instance ToJSON VideoU where
  toJSON = undefined

getChannelUploadsId :: YoutubeApiKey -> Query -> IO (Maybe Text)
getChannelUploadsId (YoutubeApiKey apiKey) forUsername = do
  let opts =
        Wreq.defaults & param "part" .~ ["contentDetails"] & pp & param "key" .~
        [apiKey]
      pp =
        case forUsername of
          Username u _  -> param "forUsername" .~ [u]
          ChannelId c _ -> param "id" .~ [c]
  r <- Wreq.getWith opts "https://www.googleapis.com/youtube/v3/channels"
  pure $ r ^? Wreq.responseBody . key "items" . nth 0 . key "contentDetails" .
    key "relatedPlaylists" .
    key "uploads" .
    _String

getChannelUploads :: YoutubeApiKey
                  -> Text
                  -> Maybe Text
                  -> IO (Maybe Text, [VideoU])
getChannelUploads (YoutubeApiKey apiKey) playlistId pageToken = do
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
      videos = jsonBody ^.. key "items" . _Array . traverse . _JSON
      tokenPart = jsonBody ^? key "nextPageToken" . _String
  pure (tokenPart, videos)

instance FromJSON Deets where
  parseJSON =
    withObject "Deets" $ \o -> do
        statistics <- o .: "statistics"
        contentDetails <- o .: "contentDetails"
        Deets <$> (contentDetails .: "duration") <*>
          (statistics .: "viewCount")

instance ToJSON Deets where
  toJSON = undefined

getVideosDetails :: YoutubeApiKey
                  -> [VideoId]
                  -> IO [Deets]
getVideosDetails (YoutubeApiKey apiKey) videoIds = do
  let opts =
        Wreq.defaults & param "part" .~ ["statistics,contentDetails"] &
        param "maxResults" .~
        ["50"] &
        param "key" .~
        [apiKey] &
        param "id" .~
        [csvIds]
      -- opts' = maybe id (\v -> param "pageToken" .~ [v]) pageToken opts
      csvIds =
        intercalate "," . fmap unVideoId $ videoIds
  r <- Wreq.getWith opts "https://www.googleapis.com/youtube/v3/videos"
  let jsonBody = r ^?! Wreq.responseBody
  let itemPart = jsonBody ^.. key "items" . _Array . traverse . _JSON
      -- tokenPart = jsonBody ^? key "nextPageToken" . _String
  pure itemPart
