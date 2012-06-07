{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Database
  ( setEncoding
  , deletePost
  , getPosts
  , getPostsCount
  , getPost
  , getPostById
  , savePost  
  
  , vaultGetPostsList
  , newPost
  , tagsToString
  , stringToTags    
  ) where 

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.Map ((!))
import Data.Time
import qualified  Database.HDBC as HDBC
import Snap.Snaplet.Hdbc hiding (query, query')

import Application()
import Types

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
  liftIO $ HDBC.commit conn
  return count    
    
setEncoding :: HasHdbc m c s => m ()
setEncoding = do
  query' "SET NAMES utf8" []
  return ()

getPosts :: HasHdbc m c s => Int -> Int -> m [Post]
getPosts offset count = do
  rows <- query 
    ("SELECT * " ++
    "FROM posts " ++ 
    "WHERE published = 1 AND special = 0 " ++
    "ORDER BY date DESC " ++
    "LIMIT ?, ?") [toSql offset, toSql count]
  return $ map rowToPost rows
     
getPostsCount :: HasHdbc m c s => m Int
getPostsCount = do
  rows <- query ("SELECT count(*) AS count " ++
    "FROM posts " ++ 
    "WHERE published = 1 AND special = 0") []
  return $ fromSql $ head rows ! "count"
  
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
savePost post@(Post pId title text url date published special tags)
  | pId == 0 = do
      query' ("INSERT INTO posts " ++ 
        "(title, text, date, url, published, special, tags) " ++ 
        "VALUES (?, ?, ?, ?, ?, ?, ?)") [
          toSql title, toSql text, toSql date, toSql url, 
          toSql published, toSql special, toSql $ tagsToString tags
        ]
      rows <- query "SELECT LAST_INSERT_ID() as id" []
      let pId' = fromSql $ head rows ! "id"
      updateTags pId' tags
      return post { postId = pId' }
  | otherwise = do
      query' ("UPDATE posts " ++ 
        "SET title = ?, text = ?, date = ?, url = ?, " ++
        "published = ?, special = ?, tags = ? " ++ 
        "WHERE id = ?") [
          toSql title, toSql text, toSql date, toSql url, 
          toSql published, toSql special, toSql $ tagsToString tags, 
          toSql pId
        ]
      updateTags pId tags
      return post  

updateTags :: HasHdbc m c s => Int -> [ByteString] -> m ()
updateTags pId tags = do
  -- TODO one transaction
  query' ("DELETE FROM posts_has_tags " ++ 
    "WHERE posts_id = ?") [toSql pId]
    
  rows <- select
  
  -- fst splitResult - already existed tags
  let splitResult = splitTagsList tags rows  
  newTags <- insertNew $ snd splitResult
  updateLinks $ map (\row -> fromSql $ row ! "id") rows ++ newTags
  return ()
  where 
    questions count = C.intercalate ", " $ replicate count "?"
    questions' count = C.intercalate ", " $ replicate count "(?, ?)"
    linkPair t = [toSql pId, toSql t]
  
    select
      | length tags > 0 =
        query ("SELECT id, tag " ++
          "FROM tags " ++
          "WHERE tag IN (" ++ C.unpack (questions $ length tags) ++ ")") $ map toSql tags
      | otherwise = return []
    
    insertNew :: HasHdbc m c s => [ByteString] -> m [Int]
    insertNew (t : ts) = do 
      query' "INSERT INTO tags (`tag`) VALUES (?)" [toSql t]
      rows <- query "SELECT LAST_INSERT_ID() as id" []
      let tagId = fromSql $ head rows ! "id"
      others <- insertNew ts 
      return (tagId : others)
    insertNew _ = return []
    

    updateLinks linksList
      | length linksList > 0 =
        query' ("INSERT INTO posts_has_tags (posts_id, tags_id) VALUES " ++ 
          C.unpack (questions' $ length linksList)) $
          concatMap linkPair linksList
      | otherwise = return 0      
  
splitTagsList :: [ByteString] -> [Row] -> ([ByteString], [ByteString])
splitTagsList tags tagRows = specify predicate tags
  where
    rows = map transformRow tagRows
    transformRow :: Row -> ByteString
    transformRow row = fromSql $ row ! "tag"
    predicate tag = tag `elem` rows

specify :: forall a. (a -> Bool) -> [a] -> ([a], [a])
specify p (x:xs) = if p x then (x : fst rest, snd rest) else (fst rest, x : snd rest)
  where rest = specify p xs
specify _ [] = ([], [])   

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
  , postTags = stringToTags $ fromSql $ rw ! "tags"
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

--
-- Utility functions
--

tagsToString :: [ByteString] -> ByteString
tagsToString = C.intercalate ", "

stringToTags :: ByteString -> [ByteString]
stringToTags str =
  let tags = map trim $ C.split ',' str in
    filter (\t -> C.length t /= 0) tags 

trim :: ByteString -> ByteString
trim = C.pack . trim' . C.unpack 

trim' :: String -> String
trim' = f . f
  where f = reverse . dropWhile isSpace                                