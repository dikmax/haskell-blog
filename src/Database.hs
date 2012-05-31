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

import Data.ByteString (ByteString)
import Data.Map ((!))
import Data.Time
import Snap.Snaplet.Hdbc
import Prelude hiding (id)

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
getPostById id = do
  rows <- query "SELECT * FROM posts WHERE id = ?" [toSql id]
  return $ rowToPost $ head rows  -- TODO check for empty result
  
savePost :: HasHdbc m c s => Post -> m Post
savePost post@(Post id title text url date published special _)
  | id == 0 = do
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
          toSql published, toSql special, SqlString "", toSql id
        ]
      return post  

deletePost :: HasHdbc m c s => ByteString -> m ()
deletePost id = do
  query' "DELETE FROM posts WHERE id = ?" [toSql id]
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
