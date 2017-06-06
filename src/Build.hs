{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Lens
import qualified Data.Map.Strict    as S
import           Data.Ord           (Down (..))
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
  ]

main :: IO ()
main = do
  apiKey <- pack <$> getEnv "YOUTUBE_API_KEY"
  (missing, built) <-
    fmap (fmap unpackBuild) $ foldMap (runUser apiKey) usersOfInterest
  print (length missing)
  print (length built)
  T.writeFile "missing.csv" (intercalate "\n" . fmap (intercalate "," . dispError) $ missing)
  renderToFile "index.html" (template usersOfInterest built)
  renderToCsv "index.csv" built
  callCommand "open -g index.html"

renderToCsv :: FilePath -> Site [Video] -> IO ()
renderToCsv fp (Site site) =
  let
      produceRows =
        S.foldMapWithKey
          (\(Down (Year y)) ->
            let y' = pack . show $ y in
             S.foldMapWithKey
               (\(Comp comp) ->
                  S.foldMapWithKey
                    (\band ->
                      let band' = pack . show $ band in
                       S.foldMapWithKey
                         (\corp'' ->
                           let corp' = pack . show $ corp'' in
                           S.foldMapWithKey (\set'' ->
                             let set''' = pack . show $ set'' in
                            fmap (\vid -> [y', comp, band', set''', corp', pack . show $ _videoTitle vid, pack . show $ _videoSource vid])
                           )))))
  in T.writeFile fp . intercalate "\n" . fmap (intercalate ",") . produceRows $ site


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
  view (_Right . to (toSite))
