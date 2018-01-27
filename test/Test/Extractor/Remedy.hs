
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Extractor.Remedy where

import           Test.QuickCheck
import           Types
-- import           Test.QuickCheck.Monadic (monadicIO, run, stop)

prop_read_sample =
  toWords "a b c" === Words ["a", "b", "c"]

return []
tests = $quickCheckAll
