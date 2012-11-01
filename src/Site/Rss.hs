{-# LANGUAGE OverloadedStrings #-}

module Site.Rss
  ( rss
  , rssSplice
  ) where

import           Prelude hiding (id)

import           Blaze.ByteString.Builder (toLazyByteString)
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Format (formatTime)
import           Data.Time.LocalTime (ZonedTime(..), minutesToTimeZone)
import           Snap.Core
import           System.Locale (rfc822DateFormat, defaultTimeLocale)
import           Text.Pandoc
import           Text.Templating.Heist
import           Text.XmlHtml

import           Application (AppHandler)
import           Config (domain)
import           Database (getPosts)
import qualified HtmlTags as H
import           HtmlTags ((<@))
import qualified HtmlAttributes as A
import           Types

rss :: AppHandler ()
rss = do
  tag <- getParam "tag"
  posts <- getPosts tag 0 10
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
    renderPost (Post id title text url date _ _ tags) = 
      Element "item" [] 
        [ Element "title" [] [TextNode title]
        , Element "link" [] [TextNode $ 
            domain `T.append` "/post/" `T.append` url]
        , Element "guid" [] [TextNode $ T.pack $ show id]
        , Element "pubDate" [] [TextNode $ T.pack $ 
            formatTime defaultTimeLocale rfc822DateFormat $
            ZonedTime date $ minutesToTimeZone 180]
        , Element "description" [] [TextNode $ T.pack (
            writeHtmlString defaultWriterOptions $ 
            readMarkdown defaultParserState $ 
            T.unpack text) `T.append` 
            "<div class=\"post-tags\"><img src=\"http://dikmax.name/img/16x16/tag_yellow.png\" /> " `T.append` 
            T.intercalate ", " tags `T.append`
            "</div>"]
        ]

-- <link rel="alternate" type="application/rss+xml" title="Лента" href="/rss"/>
rssSplice :: Maybe ByteString
          -> Splice AppHandler
rssSplice Nothing = return
  [ H.link <@ A.rel "alternate" <@ A.type_ "application/rss+xml"
    <@ A.href "/rss" <@ A.title "Лента"
  ]
rssSplice (Just tag) = return
  [ H.link <@ A.rel "alternate" <@ A.type_ "application/rss+xml"
    <@ A.href "/rss" <@ A.title "Лента всех записей"
  , H.link <@ A.rel "alternate" <@ A.type_ "application/rss+xml"
    <@ A.href ("/rss/tag/" `T.append` T.decodeUtf8 tag)
    <@ A.title ("Лента записей с тегом \"" `T.append` T.decodeUtf8 tag `T.append` "\"")
  ]
