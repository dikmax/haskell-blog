{-# LANGUAGE OverloadedStrings #-}

module Site.Rss
  ( rss
  ) where

import Blaze.ByteString.Builder (toLazyByteString)
import Data.Text (append, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Snap.Core (writeLBS)
import Text.Pandoc 
  ( defaultParserState
  , defaultWriterOptions
  , readMarkdown
  , writeHtmlString
  )
import Text.XmlHtml 
  ( Document(..)
  , Encoding(..)
  , Node(..)
  , render
  )

import Application (AppHandler)
import Config (domain)
import Database (Post(..), getLatestPosts)

rss :: AppHandler ()
rss = do 
  posts <- getLatestPosts
  writeLBS $ toLazyByteString $ render $ rssDocument posts

rssDocument :: [Post] -> Document
rssDocument posts = XmlDocument UTF8 Nothing 
  [ Element "rss" [("version", "2.0")] 
    [ Element "channel" [] $
      [ Element "title" [] [TextNode "[dikmax's blog]"]
      , Element "link" [] [TextNode $ pack domain]
      , Element "description" [] [TextNode "Мой персональный блог"]
      , Element "language" [] [TextNode "ru"]
      ] ++ map renderPost posts
    ] 
  ]
  where
    renderPost :: Post -> Node
    renderPost (Post id title text url date published special tags) = 
      Element "item" [] 
        [ Element "title" [] [TextNode $ decodeUtf8 title]
        , Element "link" [] [TextNode $ 
            pack domain `append` "/post/" `append` decodeUtf8 url]
        , Element "description" [] [TextNode $ pack $ 
            writeHtmlString defaultWriterOptions $ 
            readMarkdown defaultParserState $ 
            unpack $ decodeUtf8 text]
        ]