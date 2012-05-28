{-# LANGUAGE OverloadedStrings #-}
module Database
  ( Post(..)
  , setEncoding
  , getLatestPosts
  , getPost
  , getPostById
  , savePost
  
  , vaultGetPostsList
  , newPost    
  ) where 

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Map ((!))
import Data.Time
import qualified  Database.HDBC as HDBC
import Snap.Snaplet.Hdbc

import Application()

data Post = Post 
  { postId :: Int
  , postTitle :: ByteString
  , postText :: ByteString
  , postUrl :: ByteString
  , postDate :: LocalTime
  , postPublished :: Bool
  , postSpecial :: Bool
  , postTags :: [ByteString]
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


getPost :: HasHdbc m c s => ByteString -> m Post
getPost url = do
  rows <- noCacheQuery "SELECT * FROM posts WHERE url = ?" [toSql url]
  return $ rowToPost $ head rows  -- TODO check for empty result

getPostById :: HasHdbc m c s => ByteString -> m Post
getPostById id = do
  rows <- noCacheQuery "SELECT * FROM posts WHERE id = ?" [toSql id]
  return $ rowToPost $ head rows  -- TODO check for empty result
  
savePost :: HasHdbc m c s => Post -> m Post
savePost post@(Post id title text url date published special tags)
  | id == 0 = do
      query' ("INSERT INTO posts " ++ 
        "(title, text, date, url, published, special, tags) " ++ 
        "VALUES (?, ?, ?, ?, ?, ?, ?)") [
          toSql title, toSql text, toSql date, toSql url, 
          toSql published, toSql special, SqlString ""
        ]
      rows <- noCacheQuery "SELECT LAST_INSERT_ID() as id" []
      return post { postId = fromSql $ head rows ! "id" }
  | otherwise = do
      query' ("UPDATE posts " ++ 
        "SET title = ?, text = ?, date = ?, url = ?, " ++
        "published = ?, special = ?, tags = ? " ++ 
        "WHERE id = ?") [
          toSql title, toSql text, toSql date, toSql url, 
          toSql published, toSql special, SqlString "", toSql id
        ]
      return post  
  
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
  
newPost :: Post
newPost = Post
  { postId = 0
  , postTitle = ""
  , postText = ""
  , postDate = LocalTime -- TODO current time
    { localDay = fromGregorian 2012 01 01
    , localTimeOfDay = TimeOfDay 0 0 0
    }  
  , postUrl = ""
  , postPublished = False
  , postSpecial = False
  , postTags = []
  }   

--
-- Vault functions
--

vaultGetPostsList :: HasHdbc m c s => m [Post]
vaultGetPostsList = do
  rows <- noCacheQuery "SELECT id, title, date, published FROM posts" []
  return $ map rowToPost rows
