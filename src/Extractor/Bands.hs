{-# LANGUAGE OverloadedStrings #-}
module Extractor.Bands where

import           Control.Applicative
import           Data.List           (isPrefixOf)
import           Data.Monoid
import           Data.Semigroup
import           Data.Text           (Text, words)
import           Debug.Trace
import           Prelude             (Eq, Int, Maybe (..), otherwise, ($))
import           Types


findFirstMatch [] _ = mempty
findFirstMatch ((ws, v):rest) content
  | ws `isPrefixOf` content = v
  | otherwise = findFirstMatch rest content

is a b = (words a, Option $ Just (Max (0, b)))

isLow a b = (words a, Option $ Just (Max (-1, b)))

band :: [Text] -> Option (Max (Int, Band))
band =
  findFirstMatch
    [ "78th fraser highlanders" `is` _78thsFraserHighlanders
    , "auckland district" `is` auckland
    , "bleary & district" `is` blearyAndDistrict
    , "bleary district" `is` blearyAndDistrict -- todo dont drop ampersands
    , "bleary and district" `is` blearyAndDistrict
    , "blearly and distrct" `is` blearyAndDistrict
    , "boghall and bathgate" `is` boghall
    , "boghall bathgate" `is` boghall
    , "canterbury caledonian" `is` canterbury
    , "cullybackey pipe band" `is` cullybackey
    , "dowco triumph street" `is` dowco
    , "field marshal montgomery" `is` fmm
    , "field marshall montgomery" `is` fmm
    , "fife constabulary" `is` fifeConstabulary
    , "glasgow police" `is` glasgowPolice
    , "inveraray" `is` inverary
    , "inverary" `is` inverary
    , "la scots" `is` laScots
    , "lothian and borders police pipe band" `is` lothianAndBorders
    , "manawatu" `is` manawatu
    , "new zealand police" `is` nzpb
    , "police scotland fife" `is` policeScotlandFife
    , "peel regional police" `is` peel
    , "pipes & drums of the psni" `is` psni
    , "pipes and drums of the psni" `is` psni
    , "police service northern ireland pipe band" `is` psni
    , "police service of northern ireland pipe band" `is` psni
    , "psni pipe band" `is` psni
    , "ravara pipe band" `is` ravara
    , "scottish power" `is` sppb
    , "scottishpower" `is` sppb
    , "shotts and dykehead" `is` shotts
    , "shotts dykehead" `is` shotts
    , "simon fraser university" `is` sfu
    , "spirit of scotland" `is` sos
    , "st laurence otoole" `is` slot
    , "st lawrence otoole" `is` slot
    , "strathclyde police pipe band" `is` strathclyde
    , "toronto police" `is` toronto
    , "vale of atholl" `is` theVale
    --- Really want to match this last I think
    , "solos" `is` soloist
    , "solo" `is` soloist
    , "pipe band" `isLow` otherBand
    ]

soloist = Band "Soloists"

-- TODO: Bands have grades, but they changes Year to Year
_78thsFraserHighlanders = Band "78th Fraser Highlanders"
auckland = Band "Auckland & District Pipe Band"
blearyAndDistrict = Band "Bleary and District Pipe Band"
boghall = Band "Peoples Ford Boghall and Bathgate Caledonia"
canterbury = Band "Canterbury Caledonian Pipe Band"
cullybackey = Band "Cullybackey Pipe Band"
dowco = Band "Dowco Triumph Street Pipe Band"
fifeConstabulary = Band "Fife Constabulary Pipe Band"
fmm = Band "Field Marshal Montgomery"
glasgowPolice = Band "Glasgow Police"
inverary = Band "Inverary Pipe Band"
laScots = Band "LA Scots Pipe Band"
lothianAndBorders = Band "Lothian and Borders Police Pipe Band"
manawatu = Band "Manawatu Scottish Pipe Band"
nzpb = Band "New Zealand Police Pipe Band"
peel = Band "Peel Regional Police Pipe Band"
policeScotlandFife = Band "Police Scotland Fife"
psni = Band "Pipes and Drums of the PSNI"
ravara = Band "Ravara Pipe Band"
sfu = Band "Simon Fraser University Pipe Band"
shotts = Band "Shotts and Dykehead Caledonia"
slot = Band "St Laurence O'Toole"
sos = Band "Spirit of Scotland"
sppb = Band "Scottish Power Pipe Band"
strathclyde = Band "Strathclyde Police Pipe Band"
theVale = Band "The Vale of Atholl Pipe Band"
toronto = Band "Toronto Police Pipe Band"

otherBand = Band "Other Bands"
