{-# LANGUAGE FlexibleContexts #-}
module Site.Snaplet.DbCache where

------------------------------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.IORef
import           Data.Text (Text)
import           Snap.Snaplet
import           Snap.Snaplet.Hdbc
import           Snap.Core
import           Snap
------------------------------------------------------------------------------
import           Site.Database
import           Site.Types
------------------------------------------------------------------------------

data DbCache = DbCache
  { dcBlogs :: HashMap Text BlogData
  , dcCombinedBlogs :: HashMap Text BlogData
  } | EmptyDbCache deriving (Eq, Show)

type DbCacheRef = IORef DbCache

class HasDbCache b where
  dbCacheLens :: SnapletLens b DbCacheRef

dbCacheInit :: SnapletInit b DbCacheRef
dbCacheInit = makeSnaplet "dbCache" "Db cache snaplet" Nothing $ do
  ref <- liftIO $ newIORef EmptyDbCache
  return ref

getCache :: (HasDbCache b, HasHdbc (Handler b b) c s) => Handler b b DbCache
getCache = do
  ref <- with dbCacheLens Snap.get
  cache <- liftIO $ readIORef ref
  case cache of
    EmptyDbCache -> do
      logError "Reading cache..." -- TODO remove later
      blogs <- getBlogs
      let newCache = DbCache {
          dcBlogs = blogs
        , dcCombinedBlogs = H.empty
        }
      liftIO $ writeIORef ref newCache
      return newCache

    _ -> return cache
