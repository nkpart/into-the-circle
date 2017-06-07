{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-}

module Main where

import           Control.Lens
import           Data.Monoid
import           Data.Text          (Text, concat, intercalate, pack)
import qualified Data.Text.IO       as T
import           DrWho
import           Extractor
import           Lucid
import           Page
import           Prelude            (Either (..), Eq, FilePath, IO, Show, fmap,
                                     foldMap, length, print, pure, show, ($),
                                     (.), (<$>), (<$>), (<*>), (==))

import           System.Environment (getEnv)
import           System.Process     (callCommand)
import           Types

usersOfInterest :: [Query]
usersOfInterest =
  [ Username "dronechorus"
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
  , Username "ThereIsOnlyOneStuart"
  , Username "allynv"
  , Username "samramsaydrummer"
  , Username "pipebandfollower"
  , Username "piperbob2"
  ]

main :: IO ()
main = do
  apiKey <- pack <$> getEnv "YOUTUBE_API_KEY"
  (missing, built) <- (fmap.fmap) unpackBuild $ foldMap (runUser apiKey) usersOfInterest
  print (length missing)
  print (length built)
  T.writeFile "missed.csv" . concat . fmap ((<> "\n") . intercalate "," . dispError) $ missing
  renderToFile "index.html" (template usersOfInterest built)
  renderToFile "drummers.html" (template usersOfInterest $ filterSite (\vk -> _vidKeyCorp vk == Drum)  built)
  renderToCsv "index.csv" built
  callCommand "open -g index.html"

renderToCsv :: FilePath -> Site [Video] -> IO ()
renderToCsv fp (Site site) =
  let
      ps = pack . show
      row (VidKey (Year y) (Comp c) b co s vid) = [ps y, c, ps b, ps s, ps co, ps $ _videoTitle vid, ps $ _videoSource vid]
      produceRows =
        fmap row . fromSite
  in T.writeFile fp . concat . fmap ((<> "\n") . intercalate ",") . produceRows $ Site site

dispError :: Uncategorised -> [Text]
dispError (Uncategorised vu reason) =
    [pack (show (_videoSource vu)), _videoTitle vu, reason, videoUrl vu]

runUser :: Text -> Query -> IO ([Uncategorised], SiteBuild)
runUser apiKey u = do
  us <- cachedVideosForUser apiKey u
  let process vu = compileSite vu . extractKey $ vu
  pure $ foldMap process us

data Uncategorised =
  Uncategorised Video
                Text
  deriving (Eq, Show)

compileSite :: Video -> Either Text VidKey -> ([Uncategorised], SiteBuild)
compileSite video =
  (,) <$> view (_Left . to (pure . Uncategorised video)) <*>
  view (_Right . to (toSiteBuild))
