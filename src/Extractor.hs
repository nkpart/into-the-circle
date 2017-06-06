{-# LANGUAGE OverloadedStrings #-}

module Extractor where

import           Control.Applicative (Alternative, empty)
import           Data.Char           (isDigit)
import           Data.Foldable       (foldMap)
import           Data.List           (tails)
import           Data.Monoid         (Alt (..), (<>))
import           Data.Semigroup      (Max (..), Option (..))
import           Data.Text           (Text, all, length, unpack)
import           Data.Time
import           Extractor.Bands
import           Extractor.Comps
import           Extractor.Remedy
import           Prelude             (Either (..), Int, Maybe (..), print, pure,
                                      read, (&&), (==), (>))
import           Prelude             (IO, fromInteger, ($), (.), (<$>), (<*>))
import           Types

extractKey :: Video -> Either Text VidKey
extractKey video =
  let reader = (,,,,) <$> year <*> comp <*> band <*> set <*> corp
      (y, c, b, s, co) =
        foldMap reader (tails title) <>
        foldMap reader (tails body)
      Words title = videoTitleWords video
      Words body = videoDescriptionWords video
  in do let year' = y <.> yearOf (_videoPublishedAt video)
        (comp', band') <- mergeErrors (c <??> "No Comp") (b <???> "No Band")
        let set' = s <.> Unknown -- TODO Recover from Comp/Year (eg. British champs have been MSR comps)
            corp' = co <.> FullBand
        pure $ remedy (VidKey year' comp' band' corp' set' video)

testTitle :: Text -> IO ()
testTitle title =
      do t <- getCurrentTime
         let video = Video "ID" title "" t (Username "Testing")
         print (extractKey video)

mergeErrors :: Either Text t1 -> Either Text t -> Either Text (t1, t)
mergeErrors (Right a) (Right b) = Right (a,b)
mergeErrors (Right _) (Left e)  = Left e
mergeErrors (Left e) (Right _)  = Left e
mergeErrors (Left e1) (Left e2) = Left (e1 <> " " <> e2)

yearOf :: UTCTime -> Year
yearOf utcTime =
  let (y, _, _) = toGregorian . utctDay $ utcTime
   in Year (fromInteger y)

year :: Alternative f => [Text] -> f Year
year (h:_)
  | all isDigit h && length h == 4 && (read (unpack h) :: Int) > 1970 = pure (Year (read (unpack h)))
year _ = empty

set :: Alternative f => [Text] -> f Set
set ("msr":_)    = pure MSR
set ("medley":_) = pure Medley
set _            = empty

corp :: Alternative f => [Text] -> f Corp
corp ("drum":"corps":_) = pure Drum
corp ("pipe":"corps":_) = pure Pipe
corp ("pipers":[])      = pure Pipe
corp _                  = empty

-- SUPPORT
(<??>) :: Alt Maybe a -> t -> Either t a
Alt (Just v) <??> _ = pure v
Alt (Nothing) <??> m = Left m

(<???>) :: Option (Max (Int, a)) -> t -> Either t a
Option (Just (Max (_, b))) <???> _ = pure b
Option Nothing <???> e = Left e

(<.>) :: Alt Maybe t -> t -> t
Alt (Nothing) <.> v = v
Alt (Just v) <.> _ = v
