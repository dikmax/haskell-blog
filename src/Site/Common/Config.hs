module Site.Common.Config where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 ()
import           Database.HDBC.MySQL
------------------------------------------------------------------------------


------------------------------------------------------------------------------


-- | Db connect info
connectInfo :: MySQLConnectInfo
connectInfo = MySQLConnectInfo "127.0.0.1" "root" "" "haskellblog2" 3306 "" Nothing

-- | Default domain
defaultDomain :: ByteString
defaultDomain = "http://dikmax.test:8000/"