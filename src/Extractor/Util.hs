module Extractor.Util where

import           Control.Applicative
import           Data.Char
import           Data.Text           hiding (empty)
import           Prelude             (Int, otherwise, read, (&&), (<), (==),
                                      (>))
import           Types

textToYear :: Alternative f => Text -> f Year
textToYear h | all isDigit h &&
            length h == 4 &&
            (let x = (read (unpack h) :: Int)  in  x > 1970 && x < 2040) = pure (Year (read (unpack h)))
          | otherwise = empty
