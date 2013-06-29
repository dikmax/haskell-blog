{-# LANGUAGE OverloadedStrings #-}

module Site.Sitemap
  ( sitemap
  ) where

import Blaze.ByteString.Builder (toLazyByteString)
import qualified Data.Text as T
import Snap.Core (writeLBS)
import Text.XmlHtml
  ( Document(..)
  , Encoding(..)
  , Node(..)
  , render
  )

import Application (AppHandler)
import Config
import Database (getPosts, getTags)
import Types

sitemap :: AppHandler ()
sitemap = do
  posts <- getPosts Nothing 0 1000000
  tags <- getTags 0
  writeLBS $ toLazyByteString $ render $ sitemapDocument posts tags

sitemapDocument :: [Post] -> [Tag] -> Document
sitemapDocument posts tags = XmlDocument UTF8 Nothing
  [ Element "urlset" [("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")] $
    [ writeUrl (Just "0.5") "/"
    , writeUrl (Just "1.0") "/about"
    , writeUrl (Just "0.7") "/latest"
    , writeUrl (Just "0.1") "/archive"
    , writeUrl Nothing "/shoutbox"
    ] ++ map (writeUrl (Just "1.0") . T.append "/post/" . postUrl) posts
    ++ writePages1 Nothing "" (ceilDiv $ length posts)
    ++ concatMap (\(Tag (tag, count)) -> writePages Nothing ("/tag/" `T.append` tag) (ceilDiv count)) tags
  ]
  where
    ceilDiv count
      | count `mod` postsPerPage == 0 = count `div` postsPerPage
      | otherwise = count `div` postsPerPage + 1
    writeUrl :: Maybe T.Text -> T.Text -> Node
    writeUrl Nothing url = Element "url" []
      [ Element "loc" [] [TextNode $ domain `T.append` url]
      ]
    writeUrl (Just priority) url = Element "url" []
      [ Element "loc" [] [TextNode $ domain `T.append` url]
      , Element "priority" [] [TextNode priority]
      ]

    writePages :: Maybe T.Text -> T.Text -> Int -> [Node]
    writePages _ _ 0 = []
    writePages priority base count =
      writeUrl priority base : writePages1 priority base count

    writePages1 :: Maybe T.Text -> T.Text -> Int -> [Node]
    writePages1 _ _ 0 = []
    writePages1 _ _ 1 = []
    writePages1 priority base count =
      map (\n -> writeUrl priority $ base `T.append`
        "/page/" `T.append` T.pack (show n)) [2..count]
