module Site.Types where

import Data.Text (Text)

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