{-# LANGUAGE OverloadedStrings #-}

module Site.Common where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified HtmlTags as H
import HtmlTags ((<@), (<.), (<#))
--import HtmlTags ((<&&), (<&)
import qualified HtmlAttributes as A
import System.Locale
import Text.Templating.Heist
import Text.XmlHtml hiding (render)
import Text.XmlHtml.Cursor
import Text.Pandoc

import Application
import Config
import Types
import XmlHtmlWriter

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
renderTags tags =
  [ Element "p" []
    [ Element "span" [("itemprop", "keywords")] $ renderTags' tags ]
  ]

  where
    renderTags' (t:[]) =
      [ H.a <. "label" <@ A.href ("/tag/" `T.append` t) <# t ]
    renderTags' (t:ts) =
      [ H.a <. "label" <@ A.href ("/tag/" `T.append` t) <# t
      , TextNode " "
      ] ++ renderTags' ts
    renderTags' _ = []

renderSinglePost :: Post -> Node 
renderSinglePost post = 
  Element "article" [("class", "post")] [
    Element "h1" [("class", "post-title"), ("itemprop", "name")]
      [TextNode $ postTitle post],
    addCommentsBlock (postDate post) $ renderPostBody post "articleBody"
  ]
  where
    addCommentsBlock :: LocalTime -> Node -> Node
    addCommentsBlock time = maybe (TextNode "")
      (maybe (TextNode "") topNode . insertManyLastChild
      [ TextNode " | "
      , Element "i" [("class" , "icon-calendar")] []
      , TextNode " "
      , Element "span"
        [ ("itemprop", "dateCreated")
        , ("datetime", T.pack $ formatTime timeLocale "%Y-%m-%sT%H:%M" $ time)
        ]
        [ TextNode $ T.pack $ formatTime timeLocale "%A, %e %B %Y, %R" $ time ]
      ]) . lastChild . fromNode


-- TODO create my own writer (instead of writeHml) with blackjack and hookers  
renderPostBody :: Post -> Text -> Node
renderPostBody post propertyName =
  Element "div" [("class", "post-body"), ("itemprop", propertyName)] $
    (writeXmlHtml defaultXmlHtmlWriterOptions 
      { idPrefix = postUrl post
      , debugOutput = False
      } $
      readMarkdown parserState $ T.unpack $ postText post)
  ++ renderTags (postTags post)

parserState :: ParserState
parserState = defaultParserState 
  { stateSmart = True
  , stateParseRaw = True
  }

--
-- Metadata processing
--

data FacebookType = FacebookBlog 
  | FacebookArticle LocalTime [Text] (Maybe Text) -- Published, keywords, image
  | FacebookProfile
  | FacebookNothing

data Metadata = Metadata 
  { metaTitle :: Maybe Text
  , metaUrl :: Text
  , metaDescription :: Text
  , metaKeywords :: [Text]
  , metaType :: FacebookType
  }
metadataSplice :: Metadata -> Splice AppHandler
metadataSplice (Metadata title url description keywords ftype) =
  return $
    [ Element "title" [] [ TextNode $ titleString title]
    , Element "meta" [("name", "description"), ("content", description)] []
    , Element "meta" [("name", "keywords"), ("content", T.intercalate ", " keywords)] []
    , Element "meta" [("name", "author"), ("content", "Maxim Dikun")] []
    , Element "meta" [("property", "fb:admins"), ("content", "1201794820")] []
    , Element "meta" [("property", "og:site_name"), ("content", "[dikmax's blog]")] []
    , Element "meta" [("property", "og:title"), ("content", facebookTitleString title)] []
    , Element "meta" [("property", "og:url"), ("content", "http://dikmax.name" `T.append` if url == "" then "/" else url)] []
    , Element "meta" [("property", "og:description"), ("content", description)] []
    ] ++ (facebookType ftype)
  where 
    titleString Nothing = "[dikmax's blog]"
    titleString (Just t) = t `T.append` " :: [dikmax's blog]"

    facebookTitleString Nothing = "[dikmax's blog]"
    facebookTitleString (Just t) = t

    facebookType :: FacebookType -> [Node]
    facebookType FacebookBlog = [ Element "meta" [("property", "og:type"), ("content", "blog")] [] ]
    facebookType (FacebookArticle published tags image) = 
      [ Element "meta" [("property", "og:type"), ("content", "article")] [] 
      , Element "meta" [("property", "article:author"), ("content", "http://dikmax.name/about")] []
      , Element "meta" [("property", "article:published_time"), ("content", T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" published)] []
      , Element "meta" [("property", "article:modified_time"), ("content", T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" published)] []
      ] ++ (map (\t -> Element "meta" [("property", "article:tag"), ("content", t)] []) tags)
      ++ (maybe [] (\t -> [Element "meta" [("property", "og:image"), ("content", t)] []]) image)
    facebookType FacebookProfile =
      [ Element "meta" [("property", "og:type"), ("content", "profile")] [] 
      , Element "meta" [("property", "og:image"), ("content", "http://c358655.r55.cf1.rackcdn.com/me.jpg")] []
      , Element "meta" [("property", "profile:first_name"), ("content", "Maxim")] []
      , Element "meta" [("property", "profile:last_name"), ("content", "Dikun")] []
      , Element "meta" [("property", "profile:username"), ("content", "dikmax")] []
      , Element "meta" [("property", "profile:gender"), ("content", "male")] []
      , Element "meta" [("property", "fb:profile_id"), ("content", "1201794820")] []
      ]
    facebookType FacebookNothing = []

defaultMetadata :: Metadata
defaultMetadata = Metadata
  { metaTitle = Nothing
  , metaUrl = ""
  , metaType = FacebookNothing
  , metaDescription = "Мой личный блог"
  , metaKeywords = ["Blog", "блог"]
  }

-- Disqus vars
data DisqusVars = DisqusVars
  { disqusShortName :: Text
  , disqusIdentifier :: Maybe Text
  , disqusUrl :: Maybe Text
  , disqusTitle :: Maybe Text
  , disqusDeveloper :: Bool
  }

disqusVarsSplice :: DisqusVars -> Splice AppHandler
disqusVarsSplice (DisqusVars shortName identifier url title developer) =
  return [
    H.script <@ A.typeJavascript <#
    (
      "var disqus_shortname='" `T.append` shortName `T.append` "'" `T.append`
      (maybe "" (\v -> ",disqus_identifier='" `T.append` v `T.append` "'") identifier) `T.append`
      (maybe "" (\v -> ",disqus_url='" `T.append` v `T.append` "'") url) `T.append`
      (maybe "" (\v -> ",disqus_title='" `T.append` v `T.append` "'") title) `T.append`
      (if developer then ",disqus_developer=1" else "") `T.append` ";"
    )
  ]

defaultDisqusVars :: DisqusVars
defaultDisqusVars = DisqusVars
  { disqusShortName = "dikmax"
  , disqusIdentifier = Nothing
  , disqusUrl = Nothing
  , disqusTitle = Nothing
  , disqusDeveloper = isDeveloperMode
  }

-- | Resources revision to put in requests
resourcesRevision :: Text
resourcesRevision = "20";
