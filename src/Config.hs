{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.ByteString
import Data.ByteString.Char8 ()
import Data.Text (Text)
import Database.HDBC.MySQL

isDeveloperMode :: Bool
isDeveloperMode = False

adminLogin :: ByteString
adminLogin = "admin"

adminPassword :: ByteString
adminPassword = "admin"

domain :: Text
domain = "http://localhost:8000"

postsPerPage :: Int
postsPerPage = 5

rackspaceAuthKey :: String
rackspaceAuthKey = "00000000000000000000000000000000"

rackspaceAuthUser :: String
rackspaceAuthUser = "anonymous"

connectInfo :: MySQLConnectInfo
connectInfo = MySQLConnectInfo "127.0.0.1" "root" "" "haskellblog" 3306 "" Nothing