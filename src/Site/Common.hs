{-# LANGUAGE OverloadedStrings #-}

module Site.Common where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import System.Locale
import Text.XmlHtml hiding (render)
import Text.Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.Shared

import Types

timeLocale :: TimeLocale
timeLocale = defaultTimeLocale 
  { wDays =
    [ ("Воскресенье", "вс")
    , ("Понедельник", "пн")
    , ("Вторник", "вт")
    , ("Среда", "ср")
    , ("Четверг", "чт")
    , ("Пятница", "пт")
    , ("Суббота", "сб")
    ]
  , months = 
    [ ("января", "янв")
    , ("февраля", "фев")
    , ("марта", "мар")
    , ("апреля", "апр")
    , ("мая", "май")
    , ("июня", "июн")
    , ("июля", "июл")
    , ("августа", "авг")
    , ("сентября", "сен")
    , ("октября", "окт")
    , ("ноября", "ноя")
    , ("декабря", "дек")
    ]  
  }

renderTags :: [Text] -> [Node]
renderTags [] = []
renderTags tags = [Element "div" [("class", "post-tags")] $ renderTags' tags]
  where
    renderTags' (t:[]) = [Element "a" [("href", "/tag/" `T.append` t)]
      [TextNode t]]
    renderTags' (t:ts) = [Element "a" [("href", "/tag/" `T.append` t)]
      [TextNode t], TextNode ", "] ++ renderTags' ts
    renderTags' _ = []

renderSinglePost :: Post -> Node 
renderSinglePost post = 
  Element "div" [("class", "post")] [
    Element "p" [("class", "post-date")] 
      [TextNode $ T.pack $ formatTime timeLocale "%A, %e %B %Y, %R." $ 
        postDate post],
    Element "h1" [("class", "post-title")] 
      [TextNode $ postTitle post],
    renderPostBody post
  ]

-- TODO create my own writer (instead of writeHml) with blackjack and hookers  
renderPostBody :: Post -> Node
renderPostBody post =
  Element "div" [("class", "post-body")] $
    either (\ a -> [TextNode $ T.pack a]) extractData
  (parseHTML "post" $
     T.encodeUtf8 $
       T.pack $
         writeHtmlString writerOptions $
           readMarkdown parserState $ T.unpack $ postText post)
  ++ renderTags (postTags post)
  where
    extractData (HtmlDocument _ _ content) = content
    extractData (XmlDocument _ _ content) = content       

parserState :: ParserState
parserState = defaultParserState 
  { stateSmart = True
  , stateParseRaw = True
  }
  
writerOptions :: WriterOptions
writerOptions = defaultWriterOptions
  { writerEmailObfuscation = NoObfuscation
  , writerHighlight = True
  , writerHighlightStyle = kate
  }
