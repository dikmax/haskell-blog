{-# LANGUAGE OverloadedStrings #-}

module Site.Common where

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
-- import           Heist
import qualified Heist.Interpreted as I
-- import qualified Heist.Compiled as C
import qualified HtmlTags as H
import           HtmlTags ((<@), (<.), (<#), (<&&), (<&))
import qualified HtmlAttributes as A
import           System.Locale
import           Text.XmlHtml hiding (render)
import           Text.XmlHtml.Cursor
import           Text.Pandoc

import           Application
import           Config
import           Types
import           XmlHtmlWriter

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

-- | Simply renders post
renderSinglePost :: Bool -- ^ isAdmin
                 -> Post -- ^ postData
                 -> Node
renderSinglePost False post =
  H.article <. "post" <&
    (H.h1 <. "post-title" <@ A.itemprop "name" <# postTitle post) <&
    (H.meta <@ A.itemprop "dateCreated" <@ A.content (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" $ postDate post)) <&&
    addCommentsBlock post False (postDate post) (renderPostBody post "articleBody")
renderSinglePost True post =
  H.article <. "post" <&
    (H.h1 <. "post-title" <@ A.itemprop "name" <&&
      [ TextNode $ postTitle post
      , H.a <@ A.href ("/vault/edit/" `T.append` T.pack (show $ postId post)) <&
        H.i <. "icon-pencil"
      ]) <&
    (H.meta <@ A.itemprop "dateCreated" <@ A.content (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" $ postDate post)) <&&
    addCommentsBlock post False (postDate post) (renderPostBody post "articleBody")

addCommentsBlock :: Post -> Bool -> LocalTime -> Node -> [Node]
addCommentsBlock post commentsLink time node
  | isNothing extractTags = [ node, writeFooter Nothing ]
  | otherwise =
    [ maybe
        (TextNode "")
        (maybe (TextNode "") topNode . removeGoUp) $
        lastChild $ fromNode node
    , writeFooter extractTags
    ]
  where
    extractTags :: Maybe Node
    extractTags =
      maybe
        Nothing
        (maybe
          Nothing
          (\cur -> case getAttribute "itemprop" $ current cur of
            Just "keywords" -> Just $ current cur
            _ -> Nothing
          ) .
          firstChild
        ) $
        lastChild $ fromNode node

    writeFooter Nothing = H.p <&& writeFooter_ commentsLink
    writeFooter (Just tags) = H.p <& tags <& TextNode " | " <&& writeFooter_ commentsLink
    writeFooter_ True =
      [ H.i <. "icon-calendar"
      , TextNode " "
      , H.span <# T.pack (formatTime timeLocale "%A, %e %B %Y, %R" time)
      , TextNode " | "
      , H.i <. "icon-comment"
      , TextNode " "
      , H.span <. "post-comments"
        <& (
          H.a <@ A.href ("/post/" `T.append`
                      postUrl post `T.append` "#disqus_thread")
          <@ A.itemprop "discussionUrl"
          <# "Считаем комментарии..."
        )
      ]
    writeFooter_ False =
      [ H.i <. "icon-calendar"
      , TextNode " "
      , H.span <# T.pack (formatTime timeLocale "%A, %e %B %Y, %R" time)
      ]

-- TODO create my own writer (instead of writeHml) with blackjack and hookers  
renderPostBody :: Post -> Text -> Node
renderPostBody post propertyName =
  Element "div" [("class", "post-body"), ("itemprop", propertyName)] $
    writeXmlHtml defaultXmlHtmlWriterOptions
      { idPrefix = postUrl post
      , debugOutput = False
      }
      (readMarkdown readerOptions $ T.unpack $ postText post)
  ++ renderTags (postTags post)

readerOptions :: ReaderOptions
readerOptions = def
  { readerSmart = True
  , readerParseRaw = True
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
metadataSplice :: Metadata -> I.Splice AppHandler
metadataSplice (Metadata title url description keywords ftype) =
  return $
    -- Common metadata
    [ H.title <# titleString title
    , H.meta <@ A.name "description" <@ A.content description
    , H.meta <@ A.name "keywords" <@ A.content (T.intercalate ", " keywords)
    , H.meta <@ A.name "author" <@ A.content "Maxim Dikun"
    , H.meta <@ A.property "fb:admins" <@ A.content "1201794820"
    , H.meta <@ A.property "og:site_name" <@ A.content "[dikmax's blog]"
    , H.meta <@ A.property "og:title" <@ A.content (facebookTitleString title)
    , H.meta <@ A.property "og:url" <@ A.content urlString
    , H.meta <@ A.property "og:description" <@ A.content description
    -- Dublin Core metadata
    , H.link <@ A.rel "schema.DC" <@ A.href "http://purl.org/dc/elements/1.1/"
    , H.link <@ A.rel "schema.DCTERMS" <@ A.href "http://purl.org/dc/terms/"
    , H.meta <@ A.name "DC.title" <@ A.content (facebookTitleString title)
    , H.meta <@ A.name "DC.creator" <@ A.content "Maxim Dikun"
    , H.meta <@ A.name "DC.subject" <@ A.content (T.intercalate "; " keywords)
    , H.meta <@ A.name "DC.description" <@ A.content description
    , H.meta <@ A.name "DC.publisher" <@ A.content "Maxim Dikun"
    , H.meta <@ A.name "DC.rights" <@ A.content "Maxim Dikun, 2012"
    , H.meta <@ A.name "DC.type" <@ A.scheme "DCTERMS.DCMIType" <@ A.content "Text"
    , H.meta <@ A.name "DC.format" <@ A.content "text/html;charset=utf-8"
    , H.meta <@ A.name "DC.identifier" <@ A.scheme "DCTERMS.URI" <@ A.content urlString
    , H.meta <@ A.name "DC.language" <@ A.content "ru"
    ] ++ facebookType ftype
  where
    urlString = "http://dikmax.name" `T.append` if url == "" then "/" else url

    titleString Nothing = "[dikmax's blog]"
    titleString (Just t) = t `T.append` " :: [dikmax's blog]"

    facebookTitleString Nothing = "[dikmax's blog]"
    facebookTitleString (Just t) = t

    facebookType :: FacebookType -> [Node]
    facebookType FacebookBlog = [ H.meta <@ A.property "og:type" <@ A.content "blog" ]
    facebookType (FacebookArticle published tags image) = 
      [ H.meta <@ A.property "og:type" <@ A.content "article"
      , H.meta <@ A.property "article:author" <@ A.content "http://dikmax.name/about"
      , H.meta <@ A.property "article:published_time" <@ A.content (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" published)
      , H.meta <@ A.property "article:modified_time" <@ A.content (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" published)
      -- Dublin Core date
      , H.meta <@ A.name "DC.date" <@ A.content (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" published)
      ] ++ map (\t -> H.meta <@ A.property "article:tag" <@ A.content t) tags
      ++ maybe [] (\t -> [H.meta <@ A.property "og:image" <@ A.content t]) image
    facebookType FacebookProfile =
      [ H.meta <@ A.property "og:type" <@ A.content "profile"
      , H.meta <@ A.property "og:image" <@ A.content "http://a51056ce8d9b948fb69e-8de36eb37b2366f5a76a776c3dee0b32.r42.cf1.rackcdn.com/me.jpg"
      , H.meta <@ A.property "profile:first_name" <@ A.content "Maxim"
      , H.meta <@ A.property "profile:last_name" <@ A.content "Dikun"
      , H.meta <@ A.property "profile:username" <@ A.content "dikmax"
      , H.meta <@ A.property "profile:gender" <@ A.content "male"
      , H.meta <@ A.property "fb:profile_id" <@ A.content "1201794820"
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

disqusVarsSplice :: DisqusVars -> I.Splice AppHandler
disqusVarsSplice (DisqusVars shortName identifier url title developer) =
  return [
    H.script <@ A.typeJavascript <#
    (
      "var disqus_shortname='" `T.append` shortName `T.append` "'" `T.append`
      maybe "" (\v -> ",disqus_identifier='" `T.append` v `T.append` "'") identifier `T.append`
      maybe "" (\v -> ",disqus_url='" `T.append` v `T.append` "'") url `T.append`
      maybe "" (\v -> ",disqus_title='" `T.append` v `T.append` "'") title `T.append`
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
resourcesRevision = "26";
