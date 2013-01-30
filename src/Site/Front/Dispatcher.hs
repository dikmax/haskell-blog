{-# LANGUAGE OverloadedStrings #-}
module Site.Front.Dispatcher where

------------------------------------------------------------------------------
import           Data.ByteString.Char8 (pack)
import           Snap.Core
import           Snap.Snaplet.Heist
------------------------------------------------------------------------------
import           Application
import           Site.Snaplet.CommonData
import           Site.Snaplet.DbCache
import           Site.Types


------------------------------------------------------------------------------

dispatcher :: AppHandler ()
dispatcher = do
  blog <- getData cdBlog
  navigation <- getNavigation blog
  writeBS $ pack $ show navigation
  {-if isCommonBlog blog || isCompoundBlog blog
    then dispatchBlog $ blogId blog
    else redirect defaultDomain

  where
    dispatchBlog blogId = do
      request <- getRequest
      let path = rqPathInfo request-}
      -- TODO drop last / if present
      -- "/" `S.isSuffixOf` uri
