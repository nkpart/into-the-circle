{-# LANGUAGE OverloadedStrings #-}
module Test.Gens where

import           Data.Ord        (Down (..))
import           Extractor.Bands
import           Extractor.Comps
import           Test.QuickCheck
import           Types

genBand =
  elements [inverary, slot, OtherBand]

genComp =
  elements [ ascensionConcert, worlds, ukChampionship]

genYear :: Gen Year
genYear =
  Year <$> choose (1988, 2017)

genSet = elements [MSR, Medley, Unknown]

genCorp = elements [FullBand, Pipe, Drum]

genQuery =
  elements [ Username "SomeUser", ChannelId "GARBAGE1ABC" "Some Channel" ]

genVideo :: Gen Video
genVideo = Video <$> _ <*> _ <*> _ <*> _ <*> _

genVidKey :: Gen VidKey
genVidKey =
  VidKey <$> (Down <$> genYear) <*> genComp <*> genBand <*> genCorp <*> genSet <*> genVideo
