{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Types where

import           Control.Lens
import           Data.Char            (isAlphaNum, isDigit)
import           Data.Coerce          (coerce)
import           Data.Foldable        (foldMap)
import qualified Data.List            (filter)
import qualified Data.List            as L ()
import qualified Data.Map.Strict      as M
import           Data.Monoid          ((<>))
import           Data.Ord             (Down (..))
import           Data.Semigroup.Union (UnionWith (..))
import           Data.Sequence        (Seq, filter, fromList)
import           Data.Text            (Text, empty, toLower, words)
import qualified Data.Text            as T (filter)
import           Data.Time
import           Prelude              (Bool (..), Eq, Foldable, Int, Ord, Read,
                                       Show, fmap, not, pure, ($), (.), (=<<),
                                       (==), (||))
-- Our site is an index
type SiteBuild = Down Year :=> (Comp :=> (Band :=> (Corp :=> (Set :=> Seq Video))))
type a :=> b = UnionWith (M.Map a) b

newtype Site a = Site ([(Down Year, [(Comp, [(Band, [(Corp, [(Set, a)])])])])])
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
  { _vidKeyYear  :: Year
  , _vidKeyComp  :: Comp
  , _vidKeyBand  :: Band
  , _vidKeyCorp  :: Corp
  , _vidKeySet   :: Set
  , _vidKeyValue :: Video
  } deriving (Eq, Show)



toSiteBuild :: VidKey -> SiteBuild
toSiteBuild (VidKey a b c d e v) = (Down a `f` (b `f` (c `f` (d `f` (e `f` pure v)))))
  where
    f x y = UnionWith $ M.singleton x y

unpackBuild :: SiteBuild -> Site (Seq Video)
unpackBuild =
  Site . M.toList . fmap M.toList . (fmap.fmap) M.toList . (fmap.fmap.fmap) M.toList . (fmap.fmap.fmap.fmap) M.toList . coerce

fromSite :: Site (Seq Video) -> (Seq VidKey)
fromSite (Site s) = (=<<) z . fromList . flattenMaps . flattenMaps . flattenMaps . flattenMaps $ s
  where z (((((Down y, c), b), s'), co), v) = fmap (VidKey y c b s' co) v

filterSite :: (VidKey -> Bool) -> Site (Seq Video) -> Site (Seq Video)
filterSite p  =
  unpackBuild . foldMap toSiteBuild . filter p . fromSite

flattenMaps :: [(t1, [(t, t2)])] -> [((t1, t), t2)]
flattenMaps = (=<<) (\(k1, m2) -> fmap (\(k2,v) -> ((k1,k2),v)) $ m2)

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
toWords = Words . Data.List.filter (not . (== empty)) . fmap (T.filter (\c -> isAlphaNum c || isDigit c)) . words . toLower

videoUrl :: Video -> Text
videoUrl vid = "https://www.youtube.com/watch?v="<> _videoId vid

makeLenses ''VidKey
