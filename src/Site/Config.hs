module Site.Config where

import Database.HDBC.MySQL


-- Db connect info
connectInfo :: MySQLConnectInfo
connectInfo = MySQLConnectInfo "127.0.0.1" "root" "" "haskellblog2" 3306 "" Nothing