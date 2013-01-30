{-# LANGUAGE RankNTypes #-}
module Site.Database where

------------------------------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.List
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
  return $ foldl (\hashMap bData -> H.insert (blogDomain bData) bData hashMap) H.empty $
    map toBlogData rows

toBlogData :: forall k. (Ord k, Data.String.IsString k) => Data.Map.Map k SqlValue -> BlogData
toBlogData row = BlogData
  { blogId = fromSql $ row ! "id"
  , blogName = fromSql $ row ! "name"
  , blogDomain = fromSql $ row ! "domain"
  , blogLanguage = fromSql $ row ! "language"
  , blogUserId = fromSql $ row ! "users_id"
  }

-- Read navigation map from database
getNavigation :: HasHdbc m c s
              => m (HashMap Int BlogNavigationMap, HashMap Int BlogNavigationMap)
getNavigation = do
  rows <- query "SELECT * FROM navigation WHERE blogs_id IS NOT NULL OR combined_blogs_id IS NOT NULL" []
  let part = partition (\row -> row ! "blogs_id" /= SqlNull) rows

  return (toNavigationHashMap "blogs_id" $ fst part,
    toNavigationHashMap "combined_blogs_id" $ snd part)
  where
    toNavigationHashMap idProp rows =
      foldl (\hashMap (key, key2, value) ->
          H.insertWith H.union key (H.singleton key2 value) hashMap
        ) H.empty $
        map (toNav idProp) rows
    toNav idProp row =
      ( fromSql $ row ! idProp
      , fromSql $ row ! "url"
      , BlogNavigation
        { blogNavigationPostId = fromSql $ row ! "posts_id"
        }
      )

