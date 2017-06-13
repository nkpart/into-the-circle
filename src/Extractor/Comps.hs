{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Extractor.Comps where

import           Control.Applicative
import           Data.Text
import           Extractor.Util
import           Types

-- TODO ALMA

comp :: Alternative f => [Text] -> f Comp
comp =
  findFirstMatch
    [
      "all ireland" `is` allIreland
    , "all irelands" `is` allIreland
    , "annan" `is` annan
    , "ards" `is` ards
    , "bangor" `is` bangor
    , "belfast championships" `is` ukChampionship
    , "british championship" `is` britishChampionship
    , "british drumming champions" `is` britishChampionship
    , "british pipe band championship" `is` britishChampionship
    , "british pipe band championships" `is` britishChampionship
    , "british championships" `is` britishChampionship
    , "bathgate 2013" `is` britishChampionship
    , "dumbarton" `is` dumbarton
    , "dungannon" `is` dungannon
    , "enniskillen" `is` enniskillen
    , "european" `is` europeanChampionship
    , "europeans" `is` europeanChampionship
    , "forres" `is` forres
    , "gourock" `is` gourock
    , "newcastle" `is` newcastle
    , "north berwick" `is` northBerwick
    , "north down" `is` northDownChampionships
    , "north west championships" `is` northWestChampionships
    , "north west pipe band championships" `is` northWestChampionships
    , "piping live" `is` pipingLive
    , "portrush" `is` portrush
    , "rostrevor" `is` rostrevor
    , "scottish championships" `is` scottishChampionship
    , "scottish pipe band championships" `is` scottishChampionship
    , "uk champions" `is` ukChampionship
    , "uk championship" `is` ukChampionship
    , "uk championships" `is` ukChampionship
    , "uk pipe band championships" `is` ukChampionship
    , "united kingdom championships" `is` ukChampionship
    , "united kingdom pipe band championships" `is` ukChampionship
    , "mid ulster champions" `is` midUlsterChampionships
    , "mid ulster championships" `is` midUlsterChampionships
    , "dunbar" `is` dunbar
    , "ulster championships" `is` ulsterChampionships
    , "ulster champions" `is` ulsterChampionships
    , "vit" `is` vit
    , "world champions" `is` worlds
    , "world championships" `is` worlds
    , "world pipe band championships" `is` worlds
    , "world pipe band champiosnhips" `is` worlds
    , "world drum corp champions" `is` worlds
    , "worlds" `is` worlds
    , "glasgow green" `is` worlds
    , "live back in ireland" `is` liveBackInIreland
    , "paisley" `is` britishChampionship
    , "cookstown" `is` cookstown
    , "alma" `is` alma
    , "spring gatherin" `is` springGatherin
    , "moira" `is` moira
    , "glenarm" `is` glenarm
    , "newtonards" `is` newtownards
    , "newtownards" `is` newtownards
    , "perth" `is` perth
    , "cowal" `is` cowal
    , "markinch" `is` markinch
    , "georgetown" `is` georgetown
    ---
    , "ulster solos" `is` ulsterSolos
    , "ulster solo" `is` ulsterSolos
    --
    , "winterstorm" `is` winterstorm
    , "winter storm" `is` winterstorm
    , "inveraray ascension" `is` ascensionConcert
    , "ascension concert" `is` ascensionConcert
    , "impact" `is` impact
    , "todds bar recital" `is` toddsBarRecital
    ]

northBerwick = Comp "North Berwick"

-- Majors
britishChampionship = Comp "British Championships"
scottishChampionship = Comp "Scottish Championships"
europeanChampionship = Comp "European Championships"
worlds = Comp "Worlds"
ukChampionship = Comp "UK Championships"

-- Irish Big Ones
allIreland = Comp "All Ireland"
ulsterChampionships = Comp "Ulster Championships"
midUlsterChampionships = Comp "Mid Ulster Championships"

-- Minors and others
georgetown = Comp "Georgetown"
markinch = Comp "Markinch Highland Games"
newtownards = Comp "Newtownards"
moira = Comp "Moira"
perth = Comp "Perth"
cowal = Comp "Cowal"
glenarm = Comp "Glenarm"
springGatherin = Comp "Spring Gatherin Belfast"
alma = Comp "Alma"
annan = Comp "Annan"
ards = Comp "Ards and North Down"
bangor = Comp "Bangor"
dumbarton = Comp "Dumbarton"
dunbar = Comp "Dunbar"
dungannon = Comp "Dungannon"
enniskillen = Comp "Enniskillen"
forres = Comp "Forres" -- TODO, this is one of the big comps in some years?
gourock = Comp "Gourock"
newcastle = Comp "Newcastle"
northDown = Comp "North Down Pipe Band Championships"
northDownChampionships = Comp "North Down Pipe Band Championships"
northWestChampionships = Comp "North West Championships"
portrush = Comp "Portrush"
rostrevor = Comp "Rostrevor"
vit = Comp "Virginia International Tattoo"
cookstown = Comp "Cookstown"

-- Solo comps
ulsterSolos = Comp "Ulster Solos"

ascensionConcert = Comp "Inveraray ASCENSION"
impact = Comp "IMPACT"
pipingLive = Comp "Piping Live"
liveBackInIreland = Comp "Live Back In Ireland"
winterstorm = Comp "Winterstorm"
toddsBarRecital = Comp "Todd's Bar Recital"
