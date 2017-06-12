{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Types where

import           Control.Lens
import           Data.Char       (isAlphaNum, isDigit)
import           Data.Coerce     (coerce)
import           Data.Foldable   (foldl')
import           Data.List       (sortOn)
import qualified Data.List       (filter)
import qualified Data.List       as L ()
import qualified Data.Map.Strict as M
import           Data.Monoid     (mempty, (<>))
import           Data.Ord        (Down (..))
import           Data.Sequence   (Seq)
import           Data.Text       (Text, empty, filter, replace, toLower, words)
import qualified Data.Text       as T (filter)
import           Data.Time
import           Prelude         (Eq, Foldable, Int, Maybe (..), Ord, Read,
                                  Show, const, div, flip, fmap, fromIntegral,
                                  fst, length, not, pure, snd, ($), (+), (.),
                                  (==), (||))
-- Our site is an index

newtype Site a = Site ([(Down Year, [((Comp, UTCTime), [(Maybe Band, [(Corp, [(Set, a)])])])])])
  deriving (Eq, Show, Foldable)

------------------------------------------------------
---- Bit Types
newtype Comp =
  Comp Text
  deriving (Eq, Show, Ord)

data Band =
  Band Text | OtherBand
  deriving (Eq, Show, Ord)

shortBand :: Band -> Text
shortBand (Band b)    = replace " " "-" . filter (\x -> isAlphaNum x || x == ' ') . toLower $ b
shortBand (OtherBand) = "other-band"

longBand :: Band -> Text
longBand (Band t)  = t
longBand OtherBand = "Other Bands"

newtype Year =
  Year Int
  deriving (Eq, Show, Ord)

data Set
  = MSR
  | Medley
  | Unknown
  deriving (Eq, Show, Ord)

data Corp
  = FullBand
  | Pipe
  | Drum
  deriving (Eq, Show, Ord)

---

data Query
  = Username Text
  | ChannelId Text Text
  deriving (Eq, Ord, Read, Show)


data VideoA a = Video
  { _videoId          :: !Text
  , _videoTitle       :: !Text
  , _videoDescription :: !Text
  , _videoPublishedAt :: !UTCTime
  , _videoSource      :: !a
  } deriving (Eq, Show, Read, Ord)

type UnsourcedVideo = VideoA ()

type Video = VideoA Query

newtype Words =
  Words [Text]
  deriving (Eq, Show)

--------------------------------------------------------

data VidKey = VidKey
  { _vidKeyDownYear :: !(Down Year)
  , _vidKeyComp     :: !Comp
  , _vidKeyBand     :: !Band
  , _vidKeyCorp     :: !Corp
  , _vidKeySet      :: !Set
  , _vidKeyValue    :: !Video
  } deriving (Eq, Show, Ord)

makeLenses ''VidKey
makeLenses ''VideoA

_Down :: Simple Iso (Down a) a
_Down = iso (\(Down a) -> a) Down

type SiteBuild = Down Year :=> (Comp :=> (Maybe Band :=> (Corp :=> (Set :=> Seq Video))))
type a :=> b = M.Map a b


data BuildOpts = CollectBands
               | JustDoIt

buildSite :: Foldable f => BuildOpts -> f VidKey -> Site (Seq Video)
buildSite JustDoIt = unpackBuild . foldl' (insertVidKey Just) mempty
buildSite CollectBands = unpackBuild . foldl' (insertVidKey (const Nothing)) mempty

insertVidKey :: (Band -> Maybe Band) -> SiteBuild -> VidKey -> SiteBuild
insertVidKey f sb (VidKey a b c d e v) =
  M.insertWith (M.unionWith (M.unionWith (M.unionWith (M.unionWith (<>))))) a xxx sb
  where
    xxx = M.singleton b . M.singleton (f c) . M.singleton d . M.singleton e . pure $ v

unpackBuild :: SiteBuild -> Site (Seq Video)
unpackBuild =
  Site . M.toList . fmap (sortOn (Down . snd . fst) . fmap f' . M.toList) . (fmap.fmap) M.toList . (fmap.fmap.fmap) M.toList . (fmap.fmap.fmap.fmap) M.toList . coerce

f' :: (Comp, [(Maybe Band, [(Corp, [(Set, Seq Video)])])]) -> ((Comp, UTCTime), [(Maybe Band, [(Corp, [(Set, Seq Video)])])])
f' (c, x) =
  ((c, averageTime $ x ^.. traverse . _2 . traverse . _2 . traverse . _2 . traverse . videoPublishedAt), x)

averageTime :: [UTCTime] -> UTCTime
averageTime =
 let asNum = toModifiedJulianDay . utctDay
     avg xs =
       let r = length xs
        in foldl' (+) 0 xs `div ` (fromIntegral r)
  in flip UTCTime 0 . ModifiedJulianDay . avg . fmap asNum

------------------------------------------------------
---- Text Processing


videoTitleWords :: Video -> Words
videoTitleWords = toWords . _videoTitle

videoDescriptionWords :: Video -> Words
videoDescriptionWords = toWords . _videoDescription

toWords :: Text -> Words
toWords = Words . Data.List.filter (not . (== empty)) . fmap (T.filter (\c -> isAlphaNum c || isDigit c)) . words . toLower

videoUrl :: Video -> Text
videoUrl vid = "https://www.youtube.com/watch?v="<> _videoId vid


vidKeyYear :: Simple Lens VidKey Year
vidKeyYear = vidKeyDownYear . _Down
