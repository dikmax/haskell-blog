{-# LANGUAGE DeriveDataTypeable #-}
module Site.Types where

import Data.Data
import Data.Text (Text)
import Data.Aeson.Generic

data BlogData = BlogData
  { blogId :: Int
  , blogName :: Text
  , blogDomain :: Text
  , blogLanguage :: Text
  , blogUserId :: Int
  } deriving (Eq, Show)
data Blog
  = StandaloneBlog BlogData
  | CombinedBlog BlogData
  | UnknownBlog deriving (Eq, Show)

isStandaloneBlog :: Blog -> Bool
isStandaloneBlog (StandaloneBlog _) = True
isStandaloneBlog _ = False

isCompoundBlog :: Blog -> Bool
isCompoundBlog (CombinedBlog _) = True
isCompoundBlog _ = False

isUnknownBlog :: Blog -> Bool
isUnknownBlog UnknownBlog = True
isUnknownBlog _ = False

data NavigationPage = NavigationPage
  { navId :: Int
  , navUrl :: Text
  , navPost :: Int
  -- navOptions
  }

data PageOptions = PageOptions
  { poShowComments :: Bool
  , poKeywords :: Text
  , poDescription :: Text
  } deriving (Data, Typeable)
