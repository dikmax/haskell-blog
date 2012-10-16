{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Text
import Data.Time

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

data PostComment = PostComment
  { commentId :: Text
  , commentThread :: Int
  , commentParentId :: Maybe Text
  , commentBody :: Text
  , commentAuthorName :: Text
  , commentAuthorUrl :: Text
  , commentAuthorAvatar :: Text
  , commentDate :: LocalTime
  }

newtype Tag = Tag (Text, Int)

instance Eq Tag where
  Tag (tag1, _) == Tag (tag2, _) = tag1 == tag2

instance Ord Tag where
  compare (Tag (tag1, _)) (Tag (tag2, _)) = compare (toCaseFold tag1) (toCaseFold tag2)
