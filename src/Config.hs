{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.ByteString
import Data.ByteString.Char8 ()
import Data.Text (Text)

adminLogin :: ByteString
adminLogin = "admin"

adminPassword :: ByteString
adminPassword = "admin"

domain :: Text
domain = "http://localhost:8000"

postsPerPage :: Int
postsPerPage = 5
