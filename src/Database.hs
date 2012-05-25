{-# LANGUAGE OverloadedStrings #-}
module Database
  (
    Post(..),
    setEncoding, 
    getLatestPosts,
    getPost,
    
    vaultGetPostsList
  ) where 

import Control.Monad.IO.Class
import Data.Map ((!))
import qualified  Database.HDBC as HDBC
import Snap.Snaplet.Hdbc
import Application()
import Data.Time

data Post = Post {
  postId :: Int,
  postTitle :: String,
  postText :: String,
  postUrl :: String,
  postDate :: LocalTime,
  postPublished :: Bool,
  postSpecial :: Bool,
  postTags :: [String]
  }         

noCacheQuery
  :: HasHdbc m c s
  => String      -- ^ The raw SQL to execute. Use @?@ to indicate placeholders.
  -> [SqlValue]  -- ^ Values for each placeholder according to its position in
                 --   the SQL statement.
  -> m [Row]     -- ^ A 'Map' of attribute name to attribute value for each
                 --   row. Can be the empty list.
noCacheQuery sql bind = withTransaction $ \conn -> do
  stmt <- HDBC.prepare conn sql
  liftIO $ HDBC.execute stmt bind
  liftIO $ HDBC.fetchAllRowsMap' stmt
  
setEncoding :: HasHdbc m c s => m ()
setEncoding = do
        query' "SET NAMES utf8" []
        return ()
  
getLatestPosts :: HasHdbc m c s => m [Post]
getLatestPosts = do
  rows <- noCacheQuery "SELECT * FROM posts" []
  return $ map rowToPost rows


getPost :: HasHdbc m c s => String -> m Post
getPost postId = do
  rows <- noCacheQuery "SELECT * FROM posts WHERE url = ?" [toSql postId]
  return $ rowToPost $ head rows  -- TODO check for empty result
  
rowToPost :: Row -> Post
rowToPost rw = Post 
  { postId = fromSql $ rw ! "id"
  , postTitle = fromSql $ rw ! "title"
  , postText = fromSql $ rw ! "text"
  , postDate = fromSql $ rw ! "date"
  , postUrl = fromSql $ rw ! "url"
  , postPublished = fromSql $ rw ! "published"
  , postSpecial = fromSql $ rw ! "special"
  , postTags = [] -- TODO reading tags
  } 

--
-- Vault functions
--

vaultGetPostsList :: HasHdbc m c s => m [Post]
vaultGetPostsList = do
  rows <- noCacheQuery "SELECT id, title, date, published FROM posts" []
  return $ map rowToPost rows
