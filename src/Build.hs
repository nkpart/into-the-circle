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
-- import           Control.Monad      (unless)
import           Data.Either        (partitionEithers)
import           Data.Foldable      (fold, for_, toList)
import qualified Data.Map.Strict    as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Sequence      as S
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
  , aliasedUser "zippyzipster" "We Love Pipe Bands/ Loud Pipes Visual Media"
  , ChannelId "UC9772NGfrz4XrDN8e9wO1HQ" "StudioStabilo"
  , ChannelId "UCBlddw6QaBCtfOPDERBE4Ug" "Robert Mitchelmore"
  , ChannelId "UChy0J4ytBkPP4WV78R7ogiA" "Andrew Thomson"
  , ChannelId "UCKYUJWJ0ujZ_RZ8ZPrEZSvw" "Pipe Band TV"
  , ChannelId "UCLkhMT4HNSypw62_6i11eNQ" "topline143"
  , ChannelId "UCSDi59T263CkjC202fqQ_Ow" "Alba TV"
  , ChannelId "UCUep_ro7rvnXgGG1CqmeBDg" "Kenneth Macfarlane"
  , ChannelId "UCWUlycWU75Txr4yL7s0GWBw" "Craig Rogers"
  , ChannelId "UCZIF5RXWGD_3v5BU3N_KHRg" "RSPBA HQ"
  , ChannelId "UCZZS-Dh02rBU_PkJx7SZHWA" "ClanMacRae"
  , ChannelId "UC_8TJ5MpJhIedkoX4C_psyQ" "PBDrummerFoggo1995"
  , ChannelId "UC33u2dVbNTFSSKbmcxn1kxA" "Irish Pipe Band Photography"
  , ChannelId "UCUj6suknW_GCHbOaUc33lnQ" "MrBigAllelujia"
  , ChannelId "UCYwaZayGZP5vKouIq0w7ZqQ" "Ally Macluskie"
  , username "allynv"
  , username "celticmaps"
  , username "CrazyRed177"
  , username "drumsdotcom"
  , username "imBOSS0224"
  , username "jwramsay16"
  , username "LCK217"
  , username "MrUlsterscot"
  , username "pipebandfollower"
  , username "piperbob2"
  , username "PipesDrumsMagazine"
  , username "quickmarch"
  , username "rennieaj"
  , username "samramsaydrummer"
  , username "scotpet"
  , username "TheMillarballs"
  , username "ThereIsOnlyOneStuart"
  , username "tyfry123"
  , username "weekendsinontario"
  , username "womersleyandrew"
  ]

main :: IO ()
main = do
  apiKey <- YoutubeApiKey . pack <$> getEnv "YOUTUBE_API_KEY"
  (missing, built) <- fold <$> traverse (runUser apiKey) (take 5 usersOfInterest)
  -- let missing = S.take 5 missing'
      -- built = S.take 3 built'

  print (length missing)
  print (length built)

  let Site s = buildSite JustDoIt built
      years = s ^.. traverse . _1 . _Down
      bandsAndVids = M.toList . M.fromListWith mappend . toList . fmap (\v -> (_vidKeyBand v, S.singleton v)) $ built
      bands = fmap fst bandsAndVids
      templateBase x = contentPage x years bands

  -- Statistics
  putStrLn "Stats"
  T.writeFile "missed.csv" . foldMap ((<> "\n") . intercalate "," . dispError) $ missing
  renderToCsv "index.csv" (S.sort $ built)

  -- Index
  putStrLn "Index"
  renderToFile "docs/index.html" (indexPage usersOfInterest years bands)

  -- 1 per year
  putStrLn "Years"
  for_ s $ \(yy@(Down (Year y')), rr) -> do
    renderToFile ("docs/" <> show y' <> ".html") (templateBase ( "" <> (pack . show $ y')) (Site [(yy, rr)]))

  -- Per Band
  putStrLn "Bands"
  for_ bandsAndVids $ \(bb, vids) -> do
    let subtitle = case bb of
                      OtherBand -> "Other Bands"
                      _ | bb == soloist -> "Soloists"
                        | otherwise        -> longBand bb
    renderToFile ("docs/" <> unpack (shortBand bb) <> ".html") (templateBase subtitle (buildSite CollectBands vids))

  -- Just the drumming
  -- let justDrumming = filter (\vk -> _vidKeyCorp vk == Drum) $ DL.toList built
  -- renderToFile "docs/drummers.html" (templateBase "Drummers" $ buildSite JustDoIt justDrumming)
  putStrLn "Done"
  callCommand "open -g docs/index.html"

renderToCsv :: Foldable t => FilePath -> t VidKey -> IO ()
renderToCsv fp vids =
  let ps = pack . show
      row (VidKey (Down (Year y)) (Comp c) b co s vid) = [ps y, c, ps b, ps s, ps co, ps $ _videoTitle vid, unChannel $ _videoChannel vid]
  in T.writeFile fp . foldMap ((<> "\n") . intercalate "," . row) $ vids

dispError :: Uncategorised -> [Text]
dispError (Uncategorised vu reason) =
    [(unChannel $ _videoChannel vu), _videoTitle vu, reason, videoUrl vu]

runUser :: YoutubeApiKey -> Query -> IO (S.Seq Uncategorised, S.Seq VidKey)
runUser apiKey u = do
  print ("running-start" :: Text, u)
  us <- cacheVideoQuery apiKey u
  print ("running-end" :: Text, u)
  let process vu = (_Left %~ Uncategorised vu) . extractKey $ vu
  pure . over _2 S.fromList . over _1 S.fromList . partitionEithers . fmap process $ us

data Uncategorised =
  Uncategorised Video Text
  deriving (Eq, Show)
