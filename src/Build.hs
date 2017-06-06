{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Lens
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
  , Username "celticmaps"
  , Username "imBOSS0224"
  , ChannelId "UCZZS-Dh02rBU_PkJx7SZHWA" "ClanMacRae"
  , Username "rennieaj"
  , Username "LCK217"
  ]

main :: IO ()
main =
  do apiKey <- pack <$> getEnv "YOUTUBE_API_KEY"
     (missing, built) <- fmap (fmap unpackBuild) $ foldMap (runUser apiKey) usersOfInterest
     print (length missing)
     print (length built)
     traverse_ dispError missing
     renderToFile "index.html" (template usersOfInterest built)
     callCommand "open -g index.html"

dispError :: Uncategorised -> IO ()
dispError (Uncategorised vu reason) =
  T.putStrLn $ intercalate "," [pack (show (_videoSource vu)), _videoTitle vu, reason, videoUrl vu]

runUser :: Text -> Query -> IO ([Uncategorised], SiteBuild)
runUser apiKey u =
  do us <- cachedVideosForUser apiKey u
     let process vu = compileSite vu . extractKey $ vu
     pure $ foldMap process us

data Uncategorised = Uncategorised Video Text
  deriving (Eq, Show)

compileSite :: Video -> Either Text VidKey -> ([Uncategorised], SiteBuild)
compileSite video =
  (,) <$> view (_Left . to (pure . Uncategorised video)) <*> view (_Right . to (toSite))
