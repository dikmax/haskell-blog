{-# LANGUAGE FlexibleContexts #-}
module Site.Snaplet.DbCache where

------------------------------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.IORef
import           Data.Maybe
import           Data.Text (Text)
import           Snap.Snaplet
import           Snap.Snaplet.Hdbc
import           Snap.Core
import           Snap
------------------------------------------------------------------------------
import qualified Site.Database as Db
import           Site.Types
------------------------------------------------------------------------------

data DbCache = DbCache
  { dcBlogs :: HashMap Text BlogData
  , dcCombinedBlogs :: HashMap Text BlogData
  , dcBlogsNavigation :: HashMap Int BlogNavigationMap
  , dcCombinedBlogsNavigation :: HashMap Int BlogNavigationMap
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
      blogs <- Db.getBlogs
      combinedBlogs <- Db.getCombinedBlogs
      (blogsNav, combinedBlogsNav) <- Db.getNavigation
      let newCache = DbCache {
          dcBlogs = blogs
        , dcCombinedBlogs = combinedBlogs
        , dcBlogsNavigation = blogsNav
        , dcCombinedBlogsNavigation = combinedBlogsNav
        }
      liftIO $ writeIORef ref newCache
      return newCache

    _ -> return cache

getBlog :: (HasDbCache b, HasHdbc (Handler b b) c s) => Text -> Handler b b Blog
getBlog domain = do
  cache <- getCache
  case H.lookup domain $ dcBlogs cache of
    Just a -> return $ StandaloneBlog a
    Nothing -> case H.lookup domain $ dcCombinedBlogs cache of
      Just a -> return $ CombinedBlog a
      Nothing -> return UnknownBlog

getNavigation :: (HasDbCache b, HasHdbc (Handler b b) c s) => Blog -> Handler b b BlogNavigationMap
getNavigation (StandaloneBlog blog) = do
  cache <- getCache
  return $ fromMaybe H.empty $ H.lookup (blogId blog) $ dcBlogsNavigation cache
getNavigation (CombinedBlog blog) = do
  cache <- getCache
  return $ fromMaybe H.empty $ H.lookup (blogId blog) $ dcBlogsNavigation cache
getNavigation UnknownBlog = return H.empty
