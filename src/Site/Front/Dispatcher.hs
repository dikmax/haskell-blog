{-# LANGUAGE OverloadedStrings #-}
module Site.Front.Dispatcher where

------------------------------------------------------------------------------
import           Data.ByteString.Char8 (pack)
import qualified Data.HashMap.Strict as H
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Snap.Snaplet.Heist

------------------------------------------------------------------------------
import           Application
import           Site.Common.Config
import           Site.Snaplet.CommonData
import           Site.Snaplet.DbCache
import           Site.Types


------------------------------------------------------------------------------

dispatcher :: AppHandler ()
dispatcher = do
  blog <- getData cdBlog
  -- writeBS $ pack $ show navigation
  if not $ isUnknownBlog blog
    then dispatchBlog blog
    else redirect defaultDomain

  where
    dispatchBlog blog = do
      navigation <- getNavigation blog
      request <- getRequest
      let path = getPath $ T.decodeUtf8 $ rqPathInfo request

      maybe
        (writeBS "Not found")
        (\p -> writeBS $ pack $ show $ blogNavigationPostId p) $
        H.lookup path navigation


    getPath path
      | "/" `T.isSuffixOf` path = T.init path
      | otherwise = path

      -- TODO drop last / if present
      -- "/" `S.isSuffixOf` uri
