{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Data.Foldable      (traverse_)
import           Data.Text          (Text, intercalate, pack)
import qualified Data.Text.IO       as T
import           DrWho
import           Extractor
import           Lucid
import           Page
import           System.Environment (getEnv)
import           System.Process     (callCommand)
import           Types

usersOfInterest :: [Query]
usersOfInterest =
  [
    Username "dronechorus"
  , Username "MrUlsterscot"
  , Username "CrazyRed177"
  , ChannelId "UCWUlycWU75Txr4yL7s0GWBw" "Craig Rogers"
  , Username "drumsdotcom"
  , Username "jwramsay16"
  ]

main :: IO ()
main =
  do apiKey <- pack <$> getEnv "YOUTUBE_API_KEY"
     (missing, built) <- fmap (fmap unpackBuild) $ foldMap (runUser apiKey) usersOfInterest
     print (length missing)
     print (length built)
     traverse_ dispError missing
     renderToFile "sample.html" (template built)
     callCommand "open -g sample.html"

dispError :: (Query, Video, Text) -> IO ()
dispError (q, vu, reason) =
  T.putStrLn $ intercalate "," [pack (show q), _videoTitle vu, reason, videoUrl vu]

runUser :: Text -> Query -> IO ([(Query, Video, Text)], SiteBuild)
runUser apiKey u =
  do us <- cachedVideosForUser apiKey u
     let process vu =
          either (\l -> ([(u, vu, l)], mempty)) (\r -> (mempty, toSite r vu)) . extractKey $ vu
     pure $ foldMap process us
