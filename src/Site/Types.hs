{-# LANGUAGE DeriveDataTypeable #-}
module Site.Types where

------------------------------------------------------------------------------
import           Data.Data
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
------------------------------------------------------------------------------

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

data BlogNavigation = BlogNavigation
  { blogNavigationPostId :: Int
  -- TODO options
  } deriving (Eq, Show)

type BlogNavigationMap = HashMap Text BlogNavigation

isStandaloneBlog :: Blog -> Bool
isStandaloneBlog (StandaloneBlog _) = True
isStandaloneBlog _ = False

isCompoundBlog :: Blog -> Bool
isCompoundBlog (CombinedBlog _) = True
isCompoundBlog _ = False

isUnknownBlog :: Blog -> Bool
isUnknownBlog UnknownBlog = True
isUnknownBlog _ = False

emptyBlogData :: BlogData
emptyBlogData = BlogData 0 "" "" "" 0

extractBlogData :: Blog -> BlogData
extractBlogData (StandaloneBlog a) = a
extractBlogData (CombinedBlog a) = a
extractBlogData UnknownBlog = emptyBlogData

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
