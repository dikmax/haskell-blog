{-# LANGUAGE OverloadedStrings #-}
module Database
  (
    Post(..),
    setEncoding, 
    getLatestPosts,
    getPost
  ) where 

import Control.Monad.IO.Class
import Data.Map ((!))
import qualified  Database.HDBC as HDBC
import Snap.Snaplet.Hdbc
import Application()

data Post = Post Int String String -- id, title, text

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
  rows <- noCacheQuery "SELECT * FROM posts WHERE id = ?" [toSql postId]
  return $ rowToPost $ head rows  -- TODO check for empty result
  
rowToPost :: Row -> Post
rowToPost rw = Post (fromSql $ rw ! "id") (fromSql $ rw ! "title") (fromSql $ rw ! "text")

