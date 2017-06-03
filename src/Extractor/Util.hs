module Extractor.Util where

import           Control.Applicative
import           Data.List           (isPrefixOf)
import           Data.Text           (Text, words)
import           Prelude             (Eq, otherwise)

findFirstMatch :: (Eq k, Alternative f) => [([k], a)] -> [k] -> f a
findFirstMatch [] _ = empty
findFirstMatch ((ws, v):rest) content
  | ws `isPrefixOf` content = pure v
  | otherwise = findFirstMatch rest content

is :: Text -> t -> ([Text], t)
is a b = (words a, b)
