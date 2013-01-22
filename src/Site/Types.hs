{-# LANGUAGE DeriveDataTypeable #-}
module Site.Types where

import Data.Data
import Data.Text (Text)
import Data.Aeson.Generic

data Blog
  = StandaloneBlog
    { blogId :: Int
    , blogName :: Text
    , blogDomain :: Text
    , blogLanguage :: Text
    , blogUserId :: Int
    }
  | CombinedBlog
    { blogId :: Int
    , blogName :: Text
    , blogDomain :: Text
    , blogLanguage :: Text
    , blogUserId :: Int
    }
  | UnknownBlog deriving (Eq, Show)

isStandaloneBlog :: Blog -> Bool
isStandaloneBlog (StandaloneBlog _ _ _ _ _) = True
isStandaloneBlog _ = False

isCompoundBlog :: Blog -> Bool
isCompoundBlog (CombinedBlog _ _ _ _ _) = True
isCompoundBlog _ = False

isUnknownBlog :: Blog -> Bool
isUnknownBlog UnknownBlog = True
isUnknownBlog _ = False

data PageOptions = PageOptions
  { poShowComments :: Bool
  , poKeywords :: Text
  , poDescription :: Text
  } deriving (Data, Typeable)
