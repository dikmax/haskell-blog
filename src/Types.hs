{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.ByteString.Char8 (ByteString)
import Data.Time

-- TODO change ByteString to Text
data Post = Post 
  { postId :: Int
  , postTitle :: ByteString
  , postText :: ByteString
  , postUrl :: ByteString
  , postDate :: LocalTime
  , postPublished :: Bool
  , postSpecial :: Bool
  , postTags :: [ByteString]
  }         
