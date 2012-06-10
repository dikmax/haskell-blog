{-# LANGUAGE OverloadedStrings #-}

module Site.Rss
  ( rss
  ) where

import Prelude hiding (id)

import Blaze.ByteString.Builder (toLazyByteString)
import qualified Data.Text as T
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (ZonedTime(..), minutesToTimeZone)
import Snap.Core (writeLBS)
import System.Locale (rfc822DateFormat, defaultTimeLocale)
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
import Database (getPosts)
import Types (Post(..))

rss :: AppHandler ()
rss = do
  posts <- getPosts Nothing 0 10
  writeLBS $ toLazyByteString $ render $ rssDocument posts

rssDocument :: [Post] -> Document
rssDocument posts = XmlDocument UTF8 Nothing 
  [ Element "rss" [("version", "2.0")] 
    [ Element "channel" [] $
      [ Element "title" [] [TextNode "[dikmax's blog]"]
      , Element "link" [] [TextNode domain]
      , Element "description" [] [TextNode "Мой персональный блог"]
      , Element "language" [] [TextNode "ru"]
      ] ++ map renderPost posts
    ] 
  ]
  where
    renderPost :: Post -> Node
    renderPost (Post id title text url date _ _ _) = 
      Element "item" [] 
        [ Element "title" [] [TextNode  title]
        , Element "link" [] [TextNode $ 
            domain `T.append` "/post/" `T.append` url]
        , Element "guid" [] [TextNode $ T.pack $ show id]
        , Element "pubDate" [] [TextNode $ T.pack $ 
            formatTime defaultTimeLocale rfc822DateFormat $
            ZonedTime date $ minutesToTimeZone 180]
        , Element "description" [] [TextNode $ T.pack $ 
            writeHtmlString defaultWriterOptions $ 
            readMarkdown defaultParserState $ 
            T.unpack text]
        ]
