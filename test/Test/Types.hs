
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Types where

import           Test.Gens
import           Test.QuickCheck
import           Types

prop_ok =
  'a' === 'b'

return []
tests = $quickCheckAll
