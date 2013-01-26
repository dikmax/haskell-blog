module Site.Snaplet.DbCache where

------------------------------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.IORef
import           Data.Text (Text)
import           Snap.Snaplet
import           Snap.Core
import           Snap
------------------------------------------------------------------------------
import           Site.Types
------------------------------------------------------------------------------

data DbCache = DbCache
  { dcBlogs :: HashMap Text BlogData
  , dcCombinedBlogs :: HashMap Text BlogData
  } | EmptyDbCache

type DbCacheRef = IORef DbCache

class HasDbCache b where
  dbCacheLens :: SnapletLens b DbCacheRef

dbCacheInit :: SnapletInit b DbCacheRef
dbCacheInit = makeSnaplet "dbCache" "Db cache snaplet" Nothing $ do
  ref <- liftIO $ newIORef EmptyDbCache
  return ref
