{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Text
import Data.Time

-- TODO change ByteString to Text
data Post = Post 
  { postId :: Int
  , postTitle :: Text
  , postText :: Text
  , postUrl :: Text
  , postDate :: LocalTime
  , postPublished :: Bool
  , postSpecial :: Bool
  , postTags :: [Text]
  }         
