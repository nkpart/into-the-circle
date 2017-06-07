module Extractor.Remedy where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Extractor.Comps
import           Types

remedy :: VidKey -> VidKey
remedy a = execState m a
  where
    m = do
      -- Correct for typo
      is2106 <- uses vidKeyYear (== Year 2106)
      when is2106 $ vidKeyYear .= Year 2016

      -- Set up some Majors
      correctedComp <- liftA2 correctComp (use vidKeyComp) (use vidKeyYear)
      vidKeyComp .= correctedComp

      -- Correct Year for Concerts
      isImpact <- uses vidKeyComp (== impact)
      when isImpact $ vidKeyYear .= Year 2016

      pure ()
  -- let year = correctYear (_vidKeyYear a)
  --     cc = correctComp (_vidKeyComp a) year
  --  in a { _vidKeyYear = year, _vidKeyComp = cc }


correctYear :: Year -> Year
correctYear a
  | a == Year 2106 = Year 2016
  | otherwise = a

correctComp :: Comp -> Year -> Comp
correctComp origComp year
  | within year 2012 2016 && origComp == forres = europeanChampionship
  | within year 2010 2016 && origComp == dumbarton = scottishChampionship
  | year == Year 2017 && origComp == cookstown = midUlsterChampionships
  | otherwise = origComp

within :: Year -> Int -> Int -> Bool
within (Year y) a b = y >= a && y <= b
