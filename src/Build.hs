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
import           Data.Either        (partitionEithers)
import           Data.List          (sort)
import           Data.Monoid
import           Data.Ord
import           Data.Text          (Text, intercalate, pack)
import qualified Data.Text.IO       as T
import           DrWho
import           Extractor
import           Lucid
import           Page
import           Prelude            (Eq, FilePath, Foldable, IO, Show, filter,
                                     fmap, foldMap, length, print, pure, show,
                                     ($), (.), (<$>), (<$>), (==))
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
  , Username "weekendsinontario"
  , Username "quickmarch"
  ]

main :: IO ()
main = do
  apiKey <- pack <$> getEnv "YOUTUBE_API_KEY"
  (missing, built) <- foldMap (runUser apiKey) usersOfInterest
  print (length missing)
  print (length built)
  T.writeFile "missed.csv" . foldMap ((<> "\n") . intercalate "," . dispError) $ missing
  renderToFile "index.html" (template usersOfInterest $ buildSite built)
  let justDrumming = filter (\vk -> _vidKeyCorp vk == Drum) built
  renderToFile "drummers.html" (template usersOfInterest $ buildSite justDrumming)
  renderToCsv "index.csv" (sort built)
  callCommand "open -g index.html"

renderToCsv :: Foldable t => FilePath -> t VidKey -> IO ()
renderToCsv fp vids =
  let ps = pack . show
      row (VidKey (Down (Year y)) (Comp c) b co s vid) = [ps y, c, ps b, ps s, ps co, ps $ _videoTitle vid, ps $ _videoSource vid]
  in T.writeFile fp . foldMap ((<> "\n") . intercalate "," . row) $ vids

dispError :: Uncategorised -> [Text]
dispError (Uncategorised vu reason) =
    [pack (show (_videoSource vu)), _videoTitle vu, reason, videoUrl vu]

runUser :: Text -> Query -> IO ([Uncategorised], [VidKey])
runUser apiKey u = do
  us <- cachedVideosForUser apiKey u
  let process vu = (_Left %~ Uncategorised vu) . extractKey $ vu
  pure . partitionEithers . fmap process $ us

data Uncategorised =
  Uncategorised Video Text
  deriving (Eq, Show)
