{-# LANGUAGE MultiParamTypeClasses #-}
module Extractor.Remedy where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Extractor.Comps     hiding (is)
import           Types

remedy :: VidKey -> VidKey
remedy a = execState m a
  where
    m = do
      -- Correct for typo
      isEq vidKeyYear (Year 2106) $ vidKeyYear .= Year 2016

      is vidKeyYear (within 2012 2016) .
        isEq vidKeyComp forres $ vidKeyComp .= europeanChampionship

      is vidKeyYear (within 2010 2016) .
        isEq vidKeyComp dumbarton $ vidKeyComp .= scottishChampionship

      isEq vidKeyYear (Year 2017) .
        isEq vidKeyComp cookstown $ vidKeyComp .= midUlsterChampionships

      -- Correct Year for Concerts
      isEq vidKeyComp impact $ vidKeyYear .= Year 2016
      isEq vidKeyComp ascensionConcert $ vidKeyYear .= Year 2013

      pure ()

isEq :: (Eq a, MonadState s m) => LensLike' (Const Bool) s a -> a -> m () -> m ()
l `isEq` y =
  l `is` (== y)

is :: MonadState s m => LensLike' (Const Bool) s a -> (a -> Bool) -> m () -> m ()
l `is` y =
  \m ->
  do v <- uses l y
     when v m

within :: Int -> Int -> Year -> Bool
within  a b (Year y) = y >= a && y <= b

-- type ChangeIf a = (a -> Bool, a -> a)

-- runChanges :: [ChangeIf a] -> a -> a
-- runChanges cs initial = foldl' (\a change -> runChangeIf change a) initial cs

-- runChangeIf :: (t -> Bool, t -> t) -> t -> t
-- runChangeIf (pp, changer) v = if pp v then changer v else v
