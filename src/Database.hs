{-# LANGUAGE OverloadedStrings #-}
module Database
  (
    Post(..), 
    getLatestPosts
  ) where 

import Data.Map ((!))
import Snap.Snaplet.Hdbc
import Application()

data Post = Post Int String String -- id, title, text

getLatestPosts :: HasHdbc m c s => m [Post]
getLatestPosts = do
  rows <- query "SELECT * FROM posts" []
  return $ map rowToPost rows

rowToPost :: Row -> Post
rowToPost rw = Post (fromSql $ rw ! "id") (fromSql $ rw ! "title") (fromSql $ rw ! "text")

