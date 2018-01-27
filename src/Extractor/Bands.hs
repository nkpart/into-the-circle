{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Extractor.Bands where

import           Data.List      (isPrefixOf)
import           Data.Monoid
import           Data.Semigroup
import           Data.Text      (Text, words)
import           Prelude        (Bool, Eq, Int, Maybe (..), Num, otherwise, ($))
import           Types


findFirstMatch :: Monoid p => [(t1, t1 -> t2 -> Bool, p)] -> t2 -> p
findFirstMatch [] _ = mempty
findFirstMatch ((ws, func, v):rest) content
  | func ws content = v
  | otherwise = findFirstMatch rest content

is :: (Num p, Eq a1) => Text -> b -> ([Text], [a1] -> [a1] -> Bool, Option (Min (p, b)))
is a b = (words a, isPrefixOf, Option $ Just (Min (0, b)))

isHigh :: (Num p, Eq a1) => Text -> b -> ([Text], [a1] -> [a1] -> Bool, Option (Min (p, b)))
isHigh a b = (words a, isPrefixOf, Option $ Just (Min (-1, b)))

atPriority n a b = (words a, isPrefixOf, Option $ Just (Min (n, b)))

isLow :: (Num p, Eq a1) => Text -> b -> ([Text], [a1] -> [a1] -> Bool, Option (Min (p, b)))
isLow a b = (words a, isPrefixOf, Option $ Just (Min (1, b)))

band :: [Text] -> Option (Min (Int, Band))
band =
  findFirstMatch
    [ "78th fraser highlanders" `is` _78thsFraserHighlanders
    , "78th highlanders halifax" `is` _78thHalifax
    , "auckland district" `is` auckland
    -- , "bleary & district" `is` blearyAndDistrict
    --, "bleary district" `is` blearyAndDistrict -- todo dont drop ampersands
    -- , "bleary and district" `is` blearyAndDistrict
    -- , "blearly and district" `is` blearyAndDistrict
    -- , "blearly and distrct" `is` blearyAndDistrict
    , "bleary" `is` blearyAndDistrict
    , "blearly" `is` blearyAndDistrict
    , "boghall and bathgate" `is` boghall
    , "boghall bathgate" `is` boghall
    , "canterbury caledonian" `is` canterbury
    , "cullybackey pipe band" `is` cullybackey
    , "dowco triumph street" `is` dowco
    , "field marshal montgomery" `is` fmm
    , "field marshall montgomery" `is` fmm
    , "fm medley" `is` fmm
    , "field marshal pipers" `is` fmm
    , "fife constabulary" `is` fifeConstabulary
    , "glasgow police" `is` glasgowPolice
    , "glasgow skye" `is` glasgowSkye
    , "inveraray" `is` inveraray
    , "inverarary" `is` inveraray
    , "inverary" `is` inveraray
    , "la scots" `is` laScots
    , "lothian and borders police pipe band" `is` lothianAndBorders
    , "manawatu" `is` manawatu
    , "new zealand police" `is` nzpb
    , "police scotland fife" `is` policeScotlandFife
    , "peel regional police" `is` peel
    , "pipes drums of the psni" `is` psni
    , "police service of northern ireland" `is` psni
    -- , "police service of northern ireland pipe band" `is` psni
    , "pipes and drums of the psni" `is` psni
    , "police service northern ireland pipe band" `is` psni
    , "psni" `is` psni
    , "ravara pipe band" `is` ravara
    , "scottish power" `is` sppb
    , "scottishpower" `is` sppb
    , "shotts and dykehead" `is` shotts
    , "shotts dykehead" `is` shotts
    , "simon fraser university" `is` sfu
    , "spirit of scotland" `is` sos
    , "st laurence otoole" `is` slot
    , "st lawrence otoole" `is` slot
    , "slot" `is` slot
    , "strathclyde police pipe band" `is` strathclyde
    , "toronto police" `is` toronto
    , "vale of atholl" `is` theVale
    , "victoria police pipe band" `is` vppb
    --- Really want to match this last I think
    , "solos" `isHigh` soloist
    , "solo" `isHigh` soloist
    , "todds bar recital" `isHigh` soloist
    , "pipe idol" `isHigh` soloist
    , "pipe band" `isLow` otherBand
    , "pipes and drums" `isLow` otherBand
    ]

soloist = Band "Soloists" rotw

-- TODO: Bands have grades, but they changes Year to Year
_78thsFraserHighlanders = Band "78th Fraser Highlanders" northAmerica
_78thHalifax = Band "78th Halifax Citadel Pipe Band" northAmerica
auckland = Band "Auckland & District Pipe Band" nzAu
blearyAndDistrict = Band "Bleary and District Pipe Band" ukIreland
boghall = Band "Boghall and Bathgate Caledonia Pipe Band" ukIreland
canterbury = Band "Canterbury Caledonian Pipe Band" nzAu
cullybackey = Band "Cullybackey Pipe Band" ukIreland
dowco = Band "Dowco Triumph Street Pipe Band" northAmerica
fifeConstabulary = Band "Fife Constabulary Pipe Band" ukIreland
fmm = Band "Field Marshal Montgomery" ukIreland
glasgowPolice = Band "Glasgow Police Pipe Band" ukIreland
glasgowSkye = Band "The Glasgow Skye Association" ukIreland
inveraray = Band "Inveraray & District Pipe Band" ukIreland
laScots = Band "LA Scots Pipe Band" northAmerica
lothianAndBorders = Band "Lothian and Borders Police Pipe Band" ukIreland
manawatu = Band "Manawatu Scottish Pipe Band" nzAu
nzpb = Band "New Zealand Police Pipe Band" nzAu
peel = Band "Peel Regional Police Pipe Band" northAmerica
policeScotlandFife = Band "Police Scotland Fife" ukIreland
psni = Band "Pipes and Drums of the PSNI" ukIreland
ravara = Band "Ravara Pipe Band" ukIreland
sfu = Band "Simon Fraser University Pipe Band" northAmerica
shotts = Band "Shotts and Dykehead Caledonia" ukIreland
slot = Band "St Laurence O'Toole" ukIreland
sos = Band "Spirit of Scotland" ukIreland
sppb = Band "Scottish Power Pipe Band" ukIreland
strathclyde = Band "Strathclyde Police Pipe Band" ukIreland
theVale = Band "The Vale of Atholl Pipe Band" ukIreland
toronto = Band "Toronto Police Pipe Band" northAmerica
vppb = Band "Victoria Police Pipe Band" nzAu
otherBand = OtherBand


northAmerica = Region "North America"
ukIreland = Region "UK & Ireland"
nzAu = Region "New Zealand & Australia"
rotw = Region "Rest of the World"
