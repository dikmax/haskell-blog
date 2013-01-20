{-# LANGUAGE DoAndIfThenElse #-}
module Site.Database where

------------------------------------------------------------------------------
import           Data.Map ((!))
import           Data.Text (Text)
import qualified Database.HDBC as HDBC
import           Snap.Snaplet.Hdbc
------------------------------------------------------------------------------
import           Site.Types
------------------------------------------------------------------------------

-- | Sets MySQL connection encoding
setEncoding :: HasHdbc m c s => m ()
setEncoding = do
  query' "SET NAMES utf8" []
  return ()

getBlog :: HasHdbc m c s => Text -> m Blog
getBlog domain = do
  rows <- query "SELECT * FROM blogs WHERE domain = ?" [toSql domain]
  if length rows > 0 then
    let row = head rows in
    return $ StandaloneBlog
        { blogId = fromSql $ row ! "id"
        , blogName = fromSql $ row ! "name"
        , blogDomain = fromSql $ row ! "domain"
        , blogLanguage = fromSql $ row ! "language"
        , blogUserId = fromSql $ row ! "users_id"
        }
  else do
    rows <- query "SELECT * FROM combined_blogs WHERE domain = ?" [toSql domain]
    if length rows > 0 then
      let row = head rows in
      return $ CombinedBlog
        { blogId = fromSql $ row ! "id"
        , blogName = fromSql $ row ! "name"
        , blogDomain = fromSql $ row ! "domain"
        , blogLanguage = fromSql $ row ! "language"
        , blogUserId = fromSql $ row ! "users_id"
        }
    else
      return UnknownBlog


