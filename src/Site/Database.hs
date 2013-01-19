module Site.Database where

import qualified  Database.HDBC as HDBC
import Snap.Snaplet.Hdbc

-- | Sets MySQL connection encoding
setEncoding :: HasHdbc m c s => m ()
setEncoding = do
  query' "SET NAMES utf8" []
  return ()
