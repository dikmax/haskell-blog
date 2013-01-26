{-# LANGUAGE RankNTypes #-}
module Site.Database where

------------------------------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.Map (Map, (!))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Database.HDBC as HDBC
import           Database.HDBC.MySQL
import           Snap.Snaplet.Hdbc
------------------------------------------------------------------------------
import           Site.Types
import           Site.Common.Config
------------------------------------------------------------------------------

-- | Creating connection and setting encoding
createConnection :: IO Connection
createConnection = do
  conn <- connectMySQL connectInfo
  HDBC.runRaw conn "SET NAMES utf8"
  return conn

getBlogs :: HasHdbc m c s => m (HashMap Text BlogData)
getBlogs = do
  rows <- query "SELECT * FROM blogs" []
  return $ foldl (\h bd -> H.insert (blogDomain bd) bd h) H.empty $
    map toBlogData rows

getCombinedBlogs :: HasHdbc m c s => m (HashMap Text BlogData)
getCombinedBlogs = do
  rows <- query "SELECT * FROM combined_blogs" []
  return $ foldl (\h bd -> H.insert (blogDomain bd) bd h) H.empty $
    map toBlogData rows

toBlogData :: forall k. (Ord k, Data.String.IsString k) => Data.Map.Map k SqlValue -> BlogData
toBlogData row = BlogData
  { blogId = fromSql $ row ! "id"
  , blogName = fromSql $ row ! "name"
  , blogDomain = fromSql $ row ! "domain"
  , blogLanguage = fromSql $ row ! "language"
  , blogUserId = fromSql $ row ! "users_id"
  }
{-
getNavigation :: HasHdbc m c s => Blog -> m [NavigationPage]
getNavigation (StandaloneBlog bId _ _ _ _) = do
  rows <- query "SELECT * FROM navigation WHERE blogs_id = ?" [toSql bId]
  return $ map toNavigationPage rows
getNavigation (StandaloneBlog bId _ _ _ _) = do
  rows <- query "SELECT * FROM navigation WHERE combined_blogs_id = ?" [toSql bId]
  return $ map toNavigationPage rows
getNavigation _ = return []

toNavigationPage row = NavigationPage
  { navId = fromSql $ row ! "id"
  , navUrl = fromSql $ row ! "url"
  , navPost = fromSql $ row ! "post"
  }
-}
