{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Types where

import           Data.Char            (isAlphaNum, isDigit)
import           Data.Coerce          (coerce)
import qualified Data.List            (filter)
import qualified Data.Map.Strict      as M
import           Data.Monoid          ((<>))
import           Data.Ord             (Down (..))
import           Data.Semigroup.Union (UnionWith (..))
import           Data.Text            (Text, empty, filter, toLower, words)
import           Data.Time
import           Prelude              (Eq, Foldable, Int, Ord, Read, Show, fmap,
                                       not, ($), (.), (==), (||))
-- Our site is an index
type SiteBuild = Down Year :=> (Comp :=> (Band :=> (Corp :=> (Set :=> [Video]))))
type a :=> b = UnionWith (M.Map a) b

newtype Site a = Site (M.Map (Down Year) (M.Map Comp (M.Map Band (M.Map Corp (M.Map Set a)))))
  deriving (Eq, Show, Foldable)

------------------------------------------------------
---- Bit Types
newtype Comp =
  Comp Text
  deriving (Eq, Show, Ord)

data Band =

  Band Text | OtherBand
  deriving (Eq, Show, Ord)

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

--------------------------------------------------------

data VidKey = VidKey
  { _vidKeyYear :: Year
  , _vidKeyComp :: Comp
  , _vidKeyBand :: Band
  , _vidKeyCorp :: Corp
  , _vidKeySet  :: Set
  } deriving (Eq, Show)

toSite :: VidKey -> Video -> SiteBuild
toSite (VidKey a b c d e) v = (Down a `f` (b `f` (c `f` (d `f` (e `f` [v])))))
  where
    f x y = UnionWith $ M.singleton x y

unpackBuild :: SiteBuild -> Site [Video]
unpackBuild = coerce

------------------------------------------------------
---- Text Processing

data VideoA a = Video
  { _videoId          :: Text
  , _videoTitle       :: Text
  , _videoDescription :: Text
  , _videoPublishedAt :: UTCTime
  , _videoSource      :: a
  } deriving (Eq, Show, Read)

type UnsourcedVideo = VideoA ()

type Video = VideoA Query

videoTitleWords :: Video -> Words
videoTitleWords = toWords . _videoTitle

videoDescriptionWords :: Video -> Words
videoDescriptionWords = toWords . _videoDescription

newtype Words =
  Words [Text]
  deriving (Eq, Show)

toWords :: Text -> Words
toWords = Words . Data.List.filter (not . (== empty)) . fmap (filter (\c -> isAlphaNum c || isDigit c)) . words . toLower

videoUrl :: Video -> Text
videoUrl vid = "https://www.youtube.com/watch?v="<> _videoId vid
