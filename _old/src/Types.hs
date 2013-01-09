{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Text
import Data.Time
import System.Locale
import Text.JSON

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

instance JSON Post where
  showJSON post =
    JSObject $ toJSObject
      [ ("id", showJSON $ postId post)
      , ("title", showJSON $ postTitle post)
      -- , ("text", showJSON $ postText post)
      , ("url", showJSON $ postUrl post)
      , ("date", showJSON $ postDate post)
      , ("published", showJSON $ postPublished post)
      , ("special", showJSON $ postSpecial post)
      , ("tags", showJSON $ postTags post)
      ]
  readJSON _ = error "Not implemented"

instance JSON LocalTime where
  showJSON time = JSString $
    toJSString $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" $
      ZonedTime time $ hoursToTimeZone 3 -- TODO move timezone to config
  readJSON _ = error "Not implemented"

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
