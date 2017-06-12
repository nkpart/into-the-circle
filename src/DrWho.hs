module DrWho where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Functor.Compose
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import           Data.Time
import           System.Directory          (doesFileExist)
import           Types
import           YouTube

cacheFile :: FilePath
cacheFile = "vids.dat"

type Cache = M.Map Query (Stored [Video])

runWithCache :: StateT Cache IO a -> IO a
runWithCache  action =
  do cache <- loadCache cacheFile
     (a,r) <- runStateT action cache
     saveCache r cacheFile
     pure a
  -- load cache
  -- saveCache

cachedVideosForUser :: Text -> Query -> StateT Cache IO [Video]
cachedVideosForUser apiKey q = do
  cache <- get
  now <- liftIO $ getCurrentTime
  (vids, newCache) <- liftIO $ lookupOrRefresh now cache q (listVideosForUser apiKey q)
  put newCache
  --  saveCache newCache cacheFile
  pure vids

loadCache :: FilePath -> IO Cache
loadCache fp = do
  e <- doesFileExist fp
  if e
    then (fmap read . readFile) fp
    else pure M.empty

saveCache :: Cache -> FilePath -> IO ()
saveCache c fp = writeFile fp (show c)

lookupOrRefresh ::
     (Ord k, Monad m)
  => UTCTime
  -> M.Map k (Stored v)
  -> k
  -> m v
  -> m (v, M.Map k (Stored v))
lookupOrRefresh now inM k mv =
  getCompose $ M.alterF (Compose . getOrUpdate now mv) k inM

getOrUpdate ::
     Monad m => UTCTime -> m a -> Maybe (Stored a) -> m (a, Maybe (Stored a))
getOrUpdate now action v
  -- There are 3 cases we need to handle when choosing whether to update:
 =
  case v
    -- The key doesn't exist in the map
        of
    Nothing -> produceNewValue
    -- The key exists but is current
    Just x
      | isAlive x now -> keepExistingValue x
    -- The key does not exist in the map
      | otherwise -> produceNewValue
  where
    produceNewValue = do
      newValue <- action
      pure (newValue, Just (Stored now newValue))
    keepExistingValue x = pure (unStored x, Just (Stored now . unStored $ x))

data Stored a =
  Stored UTCTime
         a
  deriving (Eq, Show, Read)

unStored :: Stored a -> a
unStored (Stored _ a) = a

isAlive :: Stored a -> UTCTime -> Bool
isAlive (Stored t _) now = now `diffUTCTime` t < (60 * 60 * 24)
