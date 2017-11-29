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
import qualified Data.DList         as DL
import           Data.Either        (partitionEithers)
import           Data.Foldable      (fold, for_)
import           Data.List          (sort)
import qualified Data.Map.Strict    as M
import           Data.Monoid
import           Data.Ord
import           Data.Text          (Text, intercalate, pack, unpack)
import qualified Data.Text.IO       as T
import           DrWho
import           Extractor
import           Extractor.Bands
import           Lucid              hiding (for_)
import           Page
import           System.Environment (getEnv)
import           System.Process     (callCommand)
import           Types

username :: Text -> Query
username u = Username u Nothing

aliasedUser :: Text -> Text -> Query
aliasedUser a b = Username a (Just b)

usersOfInterest :: [Query]
usersOfInterest =
  [ username "dronechorus"
  , username "MrUlsterscot"
  , username "CrazyRed177"
  , ChannelId "UCWUlycWU75Txr4yL7s0GWBw" "Craig Rogers"
  , username "drumsdotcom"
  , username "jwramsay16"
  , username "celticmaps"
  , username "imBOSS0224"
  , ChannelId "UCZZS-Dh02rBU_PkJx7SZHWA" "ClanMacRae"
  , username "rennieaj"
  , username "LCK217"
  , username "ThereIsOnlyOneStuart"
  , username "allynv"
  , username "samramsaydrummer"
  , username "pipebandfollower"
  , username "piperbob2"
  , username "weekendsinontario"
  , username "quickmarch"
  , username "PipesDrumsMagazine"
  , ChannelId "UCZIF5RXWGD_3v5BU3N_KHRg" "RSPBA HQ"
  , username "womersleyandrew"
  , username "tyfry123"
  , ChannelId "UC9772NGfrz4XrDN8e9wO1HQ" "StudioStabilo"
  , aliasedUser "zippyzipster" "We Love Pipe Bands/ Loud Pipes Visual Media"
  , username "TheMillarballs"
  , ChannelId "UCKYUJWJ0ujZ_RZ8ZPrEZSvw" "Pipe Band TV"
  , ChannelId "UCBlddw6QaBCtfOPDERBE4Ug" "Robert Mitchelmore"
  , ChannelId "UCSDi59T263CkjC202fqQ_Ow" "Alba TV"
  , ChannelId "UCUep_ro7rvnXgGG1CqmeBDg" "Kenneth Macfarlane"
  , username "scotpet"
  , ChannelId "UChy0J4ytBkPP4WV78R7ogiA" "Andrew Thomson"
  , ChannelId "UCLkhMT4HNSypw62_6i11eNQ" "topline143"
  ]

main :: IO ()
main = do
  apiKey <- YoutubeApiKey . pack <$> getEnv "YOUTUBE_API_KEY"
  (missing, built) <- fold <$> traverse (runUser apiKey) usersOfInterest

  print (length missing)
  print (length built)

  let Site s = buildSite JustDoIt built
      years = s ^.. traverse . _1 . _Down
      bandsAndVids = (fmap.fmap) DL.toList . M.toList . M.fromListWith mappend . fmap (\v -> (_vidKeyBand v, DL.singleton v)) $ DL.toList built
      bands = fmap fst bandsAndVids
      templateBase = contentPage

  -- Statistics
  T.writeFile "missed.csv" . foldMap ((<> "\n") . intercalate "," . dispError) $ missing
  renderToCsv "index.csv" (sort $ DL.toList built)

  -- Index
  renderToFile "docs/index.html" (indexPage usersOfInterest years bands)

  -- 1 per year
  for_ s $ \(yy@(Down (Year y')), rr) -> do
    renderToFile ("docs/" <> show y' <> ".html") (templateBase ( "" <> (pack . show $ y')) (Site [(yy, rr)]))

  -- Per Band
  for_ bandsAndVids $ \(bb, vids) -> do
    let subtitle = case bb of
                      OtherBand -> "Showing recordings of other bands."
                      _ | bb == soloist -> "Showing recordings of soloists."
                        | otherwise        -> "Showing recordings of " <> longBand bb
    renderToFile ("docs/" <> unpack (shortBand bb) <> ".html") (templateBase subtitle (buildSite CollectBands vids))

  -- Just the drumming
  let justDrumming = filter (\vk -> _vidKeyCorp vk == Drum) $ DL.toList built
  renderToFile "docs/drummers.html" (templateBase "Drummers" $ buildSite JustDoIt justDrumming)

  callCommand "open -g docs/index.html"

renderToCsv :: Foldable t => FilePath -> t VidKey -> IO ()
renderToCsv fp vids =
  let ps = pack . show
      row (VidKey (Down (Year y)) (Comp c) b co s vid) = [ps y, c, ps b, ps s, ps co, ps $ _videoTitle vid, unChannel $ _videoChannel vid]
  in T.writeFile fp . foldMap ((<> "\n") . intercalate "," . row) $ vids

dispError :: Uncategorised -> [Text]
dispError (Uncategorised vu reason) =
    [(unChannel $ _videoChannel vu), _videoTitle vu, reason, videoUrl vu]

runUser :: YoutubeApiKey -> Query -> IO (DL.DList Uncategorised, DL.DList VidKey)
runUser apiKey u = do
  print ("running-start" :: Text, u)
  us <- cacheVideoQuery apiKey u
  print ("running-end" :: Text, u)
  let process vu = (_Left %~ Uncategorised vu) . extractKey $ vu
  pure . over _2 DL.fromList . over _1 DL.fromList . partitionEithers . fmap process $ us

data Uncategorised =
  Uncategorised Video Text
  deriving (Eq, Show)
