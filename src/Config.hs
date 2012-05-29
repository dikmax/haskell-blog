{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.ByteString
import Data.ByteString.Char8

adminLogin :: ByteString
adminLogin = "admin"

adminPassword :: ByteString
adminPassword = "admin"

domain = "http://localhost:8000"