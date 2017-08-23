{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Extractor.Comps where

import           Control.Applicative
import           Data.List           (isPrefixOf)
import           Data.Text           hiding (drop, empty, isPrefixOf, length)
import           Extractor.Util
import           Prelude             hiding (words)
import           Types
-- TODO ALMA

findFirstMatch [] _ = empty
findFirstMatch ((ws, func, v):rest) content
  | func ws content = pure v
  | otherwise = findFirstMatch rest content

is a b = (words a, isPrefixOf, b)

thisThenYear a b = (words a, prefixThenYear, b)

prefixThenYear a b =
  let matchesBeginning = a `isPrefixOf` b
      restOfB = drop (length a) b
      year = case restOfB of
              (x:_) -> textToYear x
              _     -> Nothing
   in matchesBeginning && maybe False (const True) year

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
    , "worlds grade 2" `is` worlds
    , "world championships" `is` worlds
    , "world championship" `is` worlds
    , "world pipe band championships" `is` worlds
    , "world pipe band champiosnhips" `is` worlds
    , "world drum corp champions" `is` worlds
    , "world pipe band championship" `is` worlds
    , "wWorld pipeband" `is` worlds
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
    , "lurgan" `is` lurgan
    , "lurga" `is` lurgan
    , "lochore" `is` lochore
    , "fermanagh" `is` enniskillen
    , "lisburn" `is` lisburn
    -- , "glasgow" `thisThenYear` worlds -- this gives a few false positives
    , "world" `thisThenYear` worlds
    ---
    , "ulster solos" `is` ulsterSolos
    , "ulster solo" `is` ulsterSolos
    --
    , "winterstorm" `is` winterstorm
    , "winter storm" `is` winterstorm
    , "inveraray ascension" `is` ascensionConcert
    , "ascension concert" `is` ascensionConcert
    , "motherwell concert" `is` motherwellConcert
    , "impact" `is` impact
    , "todds bar recital" `is` toddsBarRecital
    , "shotts dykehead caledonia pipe band concert" `is` shottsRise
    , "rise concert" `is` shottsRise
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
enniskillen = Comp "Enniskillen" -- TODO this is Co Fermanagh Championships
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
lurgan = Comp "Lurgan"
lochore = Comp "Lochore"
lisburn = Comp "Lisburn" -- this is hled in Moira, so maybe Moira are actually Lisburn? But there might be multiple comps.

-- Solo comps
ulsterSolos = Comp "Ulster Solos"

motherwellConcert = Comp "Motherwell Concert"
ascensionConcert = Comp "Inveraray ASCENSION"
impact = Comp "IMPACT"
pipingLive = Comp "Piping Live"
liveBackInIreland = Comp "Live Back In Ireland"
winterstorm = Comp "Winterstorm"
toddsBarRecital = Comp "Todd's Bar Recital"
shottsRise = Comp "RISE"
