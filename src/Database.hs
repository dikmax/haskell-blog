{-# LANGUAGE OverloadedStrings #-}
module Database
  ( Post(..)
  , setEncoding
  , deletePost
  , getLatestPosts
  , getPost
  , getPostById
  , savePost  
  
  , vaultGetPostsList
  , newPost    
  ) where 

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Map ((!))
import Data.Time
import qualified  Database.HDBC as HDBC
import Snap.Snaplet.Hdbc hiding (query, query')

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

query
  :: HasHdbc m c s
  => String      -- ^ The raw SQL to execute. Use @?@ to indicate placeholders.
  -> [SqlValue]  -- ^ Values for each placeholder according to its position in
                 --   the SQL statement.
  -> m [Row]     -- ^ A 'Map' of attribute name to attribute value for each
                 --   row. Can be the empty list.
query sql bind = do
  stmt <- prepare sql
  liftIO $ HDBC.execute stmt bind
  rows <- liftIO $ HDBC.fetchAllRowsMap' stmt
  liftIO $ HDBC.finish stmt
  return rows
  
query' :: HasHdbc m c s => String -> [SqlValue] -> m Integer
query' sql bind = withHdbc $ \conn -> do
  stmt <- HDBC.prepare conn sql
  count <- liftIO $ HDBC.execute stmt bind
  liftIO $ HDBC.finish stmt
  return count    
    
setEncoding :: HasHdbc m c s => m ()
setEncoding = do
  query' "SET NAMES utf8" []
  return ()
  
getLatestPosts :: HasHdbc m c s => m [Post]
getLatestPosts = do
  rows <- query 
    ("SELECT * " ++
    "FROM posts " ++ 
    "WHERE published = 1 AND special = 0 " ++
    "ORDER BY date DESC") []
  return $ map rowToPost rows


getPost :: HasHdbc m c s => ByteString -> m (Maybe Post)
getPost url = do
  rows <- query "SELECT * FROM posts WHERE url = ?" [toSql url]
  case rows of
    [] -> return Nothing
    _ -> return $ Just $ rowToPost $ head rows

getPostById :: HasHdbc m c s => ByteString -> m Post
getPostById pId = do
  rows <- query "SELECT * FROM posts WHERE id = ?" [toSql pId]
  return $ rowToPost $ head rows  -- TODO check for empty result
  
savePost :: HasHdbc m c s => Post -> m Post
savePost post@(Post pId title text url date published special _)
  | pId == 0 = do
      query' ("INSERT INTO posts " ++ 
        "(title, text, date, url, published, special, tags) " ++ 
        "VALUES (?, ?, ?, ?, ?, ?, ?)") [
          toSql title, toSql text, toSql date, toSql url, 
          toSql published, toSql special, SqlString ""
        ]
      rows <- query "SELECT LAST_INSERT_ID() as id" []
      return post { postId = fromSql $ head rows ! "id" }
  | otherwise = do
      query' ("UPDATE posts " ++ 
        "SET title = ?, text = ?, date = ?, url = ?, " ++
        "published = ?, special = ?, tags = ? " ++ 
        "WHERE id = ?") [
          toSql title, toSql text, toSql date, toSql url, 
          toSql published, toSql special, SqlString "", toSql pId
        ]
      return post  

deletePost :: HasHdbc m c s => ByteString -> m ()
deletePost pId = do
  query' "DELETE FROM posts WHERE id = ?" [toSql pId]
  return ()
  
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
  rows <- query 
    ("SELECT id, title, date, url, published " ++
     "FROM posts " ++ 
     "ORDER BY date DESC") []
  return $ map rowToPost rows
