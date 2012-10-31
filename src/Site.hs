{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.List (groupBy, maximumBy, minimumBy, sort)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Database.HDBC.MySQL
import           Prelude
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           System.Locale
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
import           Text.XmlHtml.Cursor
------------------------------------------------------------------------------
import           Application
import           Config
import           Database
import qualified HtmlTags as H
import           HtmlTags ((<@), (<.), (<#), (<&), (<&&))
import qualified HtmlAttributes as A
import           Site.Common
import           Site.Rss
import           Site.Sitemap
import           Site.Vault
import           Site.Vault.Files
import           Site.Utils
import           Types

------------------------------------------------------------------------------
index :: Handler App App ()
index =  ifTop $ do
  page <- decodedParam "page"
  tag <- getParam "tag"
  let
    tagText = T.decodeUtf8' $ fromMaybe "" tag
    tagLink
      | tag == Nothing = Nothing
      | otherwise = case tagText of
        (Left _) -> Nothing
        (Right t) -> Just t
    pageNum = case reads $ unpack page of
      [(x, "")] -> x
      _ -> 1
    indexSplices = 
      [ ("posts", postsSplice pageNum tag) 
      , ("pagination", paginationSplice pageNum tag)
      , ("metadata", metadataSplice $ defaultMetadata 
        { metaUrl = maybe "" (\t -> "/tag/" `T.append` t) tagLink
            `T.append` if pageNum == 1 then "" else "/page/" `T.append` T.pack (show pageNum)
        , metaDescription = makeDescription tagLink pageNum
        , metaTitle = makeTitle tagLink pageNum
        , metaType = FacebookBlog
        })
      ]

    makeDescription Nothing 1 = "Мой персональный блог. Я рассказываю о программировании и иногда о своей жизни."
    makeDescription Nothing p = "Мой персональный блог, записи с " `T.append`
      T.pack (show ((p - 1) * postsPerPage + 1)) `T.append` " по " `T.append`
      T.pack (show (p * postsPerPage)) `T.append` "."
    makeDescription (Just t) 1 = "Мой персональный блог, записи с тегом \"" `T.append` t `T.append` ".\""
    makeDescription (Just t) p = "Мой персональный блог, записи с тегом \"" `T.append` t `T.append` "\" c " `T.append`
      T.pack (show ((p - 1) * postsPerPage + 1)) `T.append` " по " `T.append`
      T.pack (show (p * postsPerPage))`T.append` "."

    makeTitle Nothing 1 = Nothing
    makeTitle Nothing p = Just $ T.pack (show p) `T.append` "-я страница"
    makeTitle (Just t) 1 = Just $ "\"" `T.append` t `T.append` "\""
    makeTitle (Just t) p = Just $ "\"" `T.append` t `T.append` "\", " `T.append` T.pack (show p) `T.append`
      "-я страница"

  either
    (\_ -> error404)
    (\_ -> heistLocal (bindSplices indexSplices) $ render "index")
    tagText

postsSplice :: Int -> Maybe ByteString -> Splice AppHandler
postsSplice page tag = do
  posts <- lift $ getPosts tag ((page - 1) * postsPerPage) postsPerPage
  return [H.div <. "posts" <&& map renderPostInList posts]

paginationSplice :: Int -> Maybe ByteString -> Splice AppHandler
paginationSplice page tag = do
  postsCount <- lift $ getPostsCount tag
  let
    prevDisabled = page * postsPerPage >= postsCount  
    
    prevLink :: Text
    prevLink = maybe "" (\t -> "/tag/" `T.append` T.decodeUtf8 t) tag
      `T.append` "/page/" `T.append` T.pack (show (page + 1))
    
    nextDisabled = page <= 1

    nextLink :: Text
    nextLink = if nextLink_ == "" then "/" else nextLink_

    nextLink_ :: Text
    nextLink_ = maybe "" (\t -> "/tag/" `T.append` T.decodeUtf8 t) tag
      `T.append`  if page == 2 then "" else "/page/" `T.append` 
        T.pack (show (page - 1))

    prevElement = if prevDisabled
      then []
      else [H.li <. "previous" <& (H.a <@ A.href prevLink <# "← Старше") ]
    nextElement = if nextDisabled      
      then []
      else [H.li <. "next" <& (H.a <@ A.href nextLink <# "Моложе →") ]
  return [H.ul <. "pager" <&& prevElement ++ nextElement]
        
renderPostInList :: Post -> Node 
renderPostInList post = 
  H.article
    <. "post component-panel"
    <@ A.itemprop "blogPost"
    <@ A.itemscope
    <@ A.itemtype "http://schema.org/BlogPosting"
    <&& [
    H.div <@ A.itemprop "author" <@ A.itemscope <@ A.itemtype "http://schema.org/Person" <&&
      [ H.meta <@ A.itemprop "name" <@ A.content "Maxim Dikun"
      , H.link <@ A.itemprop "url" <@ A.content "http://dikmax.name/about"
      , H.link <@ A.itemprop "url" <@ A.content "https://plus.google.com/109129288587536990618/posts"
      ],
    H.h1 <. "post-title" <@ A.itemprop "name" <& (
      H.a <@ A.href ("/post/" `T.append` postUrl post) <@ A.itemprop "url"
        <# postTitle post
    ),
    addCommentsBlock (postDate post) $ renderPostBody post "articleBody"
  ]
  where
    addCommentsBlock :: LocalTime -> Node -> Node
    addCommentsBlock time = maybe (TextNode "")
      (maybe (TextNode "") topNode . insertManyLastChild
      [ TextNode " | "
      , H.i <. "icon-calendar"
      , TextNode " "
      , H.span
        <@ A.itemprop "dateCreated"
        <@ A.datetime (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" time)
        <# T.pack (formatTime timeLocale "%A, %e %B %Y, %R" time)
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
      ]) . lastChild . fromNode

-- |
-- Show post Action
--
showPost :: AppHandler ()
showPost = do
  url <- decodedParam "post"
  post <- getPost url
  comments <- getComments post
  maybe error404 
    (\p -> heistLocal (bindSplices 
      [ ("post", return [renderResult p])
      , ("comments", commentsSplice comments)
      , ("metadata", metadataSplice $ defaultMetadata 
        { metaTitle = Just $ postTitle p
        , metaUrl = "/post/" `T.append` T.decodeUtf8 url
        , metaType = FacebookArticle (postDate p) (postTags p) (getImage $ renderResult p)
        , metaDescription = getDescription $ renderResult p
        })
      , ("disqusVars", disqusVarsSplice $ defaultDisqusVars
        { disqusIdentifier = Just $ T.decodeUtf8 url
        , disqusUrl = Just $ "http://dikmax.name/post/" `T.append` T.decodeUtf8 url
        , disqusTitle = Just $ postTitle p
        })
      ]) $ render "post") post
  where
    renderResult = renderSinglePost

    emptyDescription = "Мой персональный блог"

    -- filter .post-tags
    getDescription :: Node -> Text
    getDescription = maybe emptyDescription 
      (until (not . T.null) (\_ -> emptyDescription) . getDescription') . 
      findChild (checkMainDiv . current) . fromNode
    checkMainDiv node = maybe False (== "div") (tagName node) &&
      maybe False (== "articleBody") (getAttribute "itemprop" node)

    getDescription' :: Cursor -> Text
    getDescription' = cutDescription . transformDescription .
      T.intercalate " " . map nodeText . filter checkParagraph .
      maybe [] siblings . firstChild
    checkParagraph = maybe False (`elem` ["p", "h2", "h3", "h4", "h5", "h6"]) . tagName

    transformDescription = T.replace "\n" " "
    cutDescription d
      | T.length d > 512 = T.stripEnd (fst $ T.breakOnEnd " " $ T.take 512 d) `T.append` "..."
      | otherwise = d

    getImage :: Node -> Maybe Text
    getImage = 
      maybe Nothing (        
        maybe Nothing (getAttribute "src" . current) . 
        findChild (maybe False (== "img") . tagName . current)
      ) . findRec (checkFigure . current) . fromNode      
    checkFigure node = maybe False (== "div") (tagName node) &&
      maybe False (== "figure") (getAttribute "class" node)

renderComments :: [PostComment] -> [Node]
renderComments comments =
  [H.div <. "post-comments" <&& map commentToHtml comments]
  where
    commentToHtml comment =
      H.div <. "post-comment" <@ A.itemprop "comment" <@ A.itemtype "http://schema.org/UserComments" <@ A.itemscope
        <&&
        [ H.img <. "post-comment-avatar" <@ A.src (commentAuthorAvatar comment)
          <@ A.itemprop "image" <@ A.href (commentAuthorAvatar comment)
          <@ A.alt (commentAuthorName comment) <@ A.title (commentAuthorName comment)
        , H.div <. "post-comment-body"
            <& (
              H.header <&&
                [ H.span <. "post-comment-author" <@ A.itemprop "creator" <@ A.itemtype "http://schema.org/Person" <@ A.itemscope <&&
                  [ H.meta <@ A.itemprop "name" <@ A.content (commentAuthorName comment)
                  , if commentAuthorUrl comment == ""
                    then TextNode $ commentAuthorName comment
                    else H.a <@ (A.href $ commentAuthorUrl comment) <# commentAuthorName comment
                  ]
                , H.span <. "post-comment-bullet" <@ ("aria-hidden", "true") <# "•"
                , H.span <. "post-comment-date" <@ A.itemprop "commentTime"
                  <@ A.datetime (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M:%S" $ commentDate comment)
                  <# T.pack (formatTime timeLocale "%A, %e %B %Y, %R" $ commentDate comment)
                ]
            ) <& (H.div <@ A.itemprop "commentText" <&& (toComment $ parseHTML "comment.html" $ T.encodeUtf8 $ commentBody comment))
        , H.div <. "clearfix"
        ]
    toComment :: Either String Document -> [Node]
    toComment (Left _) = []
    toComment (Right (XmlDocument _ _ nodes)) = nodes
    toComment (Right (HtmlDocument _ _ nodes)) = nodes

commentsSplice :: [PostComment] -> Splice AppHandler
commentsSplice = return . renderComments

-- |
-- About me action
--
aboutMe :: AppHandler ()
aboutMe = heistLocal (bindSplices
  [ ("about", aboutSplice)
  , ("metadata", metadataSplice $ defaultMetadata 
    { metaTitle = Just "Обо мне"
    , metaDescription = "Здравствуйте, очень приятно, царь"
    , metaUrl = "/about"
    , metaType = FacebookProfile
    })
  , ("disqusVars", disqusVarsSplice $ defaultDisqusVars
    { disqusIdentifier = Just "about"
    , disqusUrl = Just  "http://dikmax.name/about"
    , disqusTitle = Just "Обо мне"
    })
  ] ) $ render "about"
  
aboutSplice :: Splice AppHandler
aboutSplice = do
  post <- lift $ getPost "about"
  return $ maybe [] (\p -> [renderPostBody p "mainContentOfPage"]) post

-- |
-- Shoutbox action
--
shoutbox :: AppHandler ()
shoutbox = heistLocal (bindSplices
  [ ("shoutbox", shoutboxSplice)
  , ("metadata", metadataSplice $ defaultMetadata 
    { metaTitle = Just "Shoutbox"
    , metaDescription = "Специальнaя страница, где можно пошуметь и прокричать все, что угодно"
    , metaUrl = "/shoutbox"
    })
  , ("disqusVars", disqusVarsSplice $ defaultDisqusVars
    { disqusIdentifier = Just "shoutbox"
    , disqusUrl = Just  "http://dikmax.name/shoutbox"
    , disqusTitle = Just "Shoutbox"
    })
  ] ) $ render "shoutbox"
  
shoutboxSplice :: Splice AppHandler
shoutboxSplice = do
  post <- lift $ getPost "shoutbox"
  comments <- lift $ getComments post
  return $ maybe [] (\p ->
    [ renderPostBody p "mainContentOfPage"
    , H.div <@ ("id", "disqus_thread") <&& renderComments comments
    ]) post

-- |
-- Archive action
--
archive :: AppHandler ()
archive = heistLocal (bindSplices
  [ ("archive", archiveSplice)
  , ("metadata", metadataSplice $ defaultMetadata
    { metaTitle = Just "Архив"
    , metaDescription = "Список всех постов для \"быстрого поиска\""
    , metaUrl = "/archive"
    })
  ] ) $ render "archive"

archiveSplice :: Splice AppHandler
archiveSplice = do
  posts <- lift $ getPosts Nothing 0 1000

  return [H.ul <. "media-list" <&& renderList posts]
  where
    renderList posts =
      concat $ map (\list -> (H.h2 <. "archive-month" <# T.pack (formatTime archiveLocale "%B %Y" $ postDate $ head list)) : map renderPost list) $
        groupBy (\a b -> isEqual (toGregorian $ localDay $ postDate a) (toGregorian $ localDay $ postDate b)) posts
    archiveLocale = defaultTimeLocale
      { months =
        [ ("Январь", "янв")
        , ("Февраль", "фев")
        , ("Март", "мар")
        , ("Апрель", "апр")
        , ("Май", "май")
        , ("Июнь", "июн")
        , ("Июль", "июл")
        , ("Август", "авг")
        , ("Сентябрь", "сен")
        , ("Октябрь", "окт")
        , ("Ноябрь", "ноя")
        , ("Декабрь", "дек")
        ]
      }
    isEqual (y1, m1, _) (y2, m2, _) = y1 == y2 && m1 == m2
    getDay (_, _, d) = T.pack $ show d

    renderPost post =
      H.li <. "media" <@ A.itemtype "http://schema.org/BlogPosting" <@ A.itemscope <@ A.itemprop "blogPost" <&&
      [ H.div <@ A.itemprop "author" <@ A.itemscope <@ A.itemtype "http://schema.org/Person" <&&
        [ H.meta <@ A.itemprop "name" <@ A.content "Maxim Dikun"
        , H.link <@ A.itemprop "url" <@ A.content "http://dikmax.name/about"
        , H.link <@ A.itemprop "url" <@ A.content "https://plus.google.com/109129288587536990618/posts"
        ]
      , H.meta <@ A.itemprop "dateCreated" <@ A.content (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" $ postDate post)
      , H.span <. "pull-left"  <&
        (
          H.span <. "media-object archive-day" <# (getDay $ toGregorian $ localDay $ postDate post)
        )
      , H.h4 <. "media-heading" <@ A.itemprop "name" <&
        (
          H.a <@ A.itemprop "url" <@ A.href ("/post/" `T.append` postUrl post) <# postTitle post
        )
      , H.div <&& renderTags (postTags post)
      ]

-- |
-- Latest movies action
--
latestMovies :: AppHandler ()
latestMovies = heistLocal (bindSplices
  [ ("latest", latestMoviesSplice)
  , ("metadata", metadataSplice $ defaultMetadata 
    { metaTitle = Just "Последние просмотренные фильмы"
    , metaDescription = "Последние просмотренные фильмы"
    , metaUrl = "/latest"
    })
  , ("disqusVars", disqusVarsSplice $ defaultDisqusVars
    { disqusIdentifier = Just "latest"
    , disqusUrl = Just  "http://dikmax.name/latest"
    , disqusTitle = Just "Последние просмотренные фильмы"
    })
  ] ) $ render "latest"
  
latestMoviesSplice :: Splice AppHandler
latestMoviesSplice = do
  post <- lift $ getPost "latest"
  comments <- lift $ getComments post
  return $ maybe [] (\p ->
    [ renderPostBody p "mainContentOfPage"
    , H.div <@ ("id", "disqus_thread") <&& renderComments comments
    ]) post

--
-- Navigation
-- 
siteStructure :: [(Text, Text)]
siteStructure = 
  [ ("Обо мне", "/about")
  , ("Shoutbox", "/shoutbox")
  , ("Архив", "/archive")
  , ("Фильмы", "/latest")
  ]

createList :: Text -> [Node]
createList _ = map listItem siteStructure
  where
    listItem (title, url) = H.li <& H.a <@ A.href url <# title

themesList :: [(Text, Text)]
themesList = 
  [ ]

createThemesList :: [Node]
createThemesList = concatMap listItem themesList
  where
    listItem :: (Text, Text) -> [Node]
    listItem (title, url) = 
      [ H.a <. "main-theme-link" <@ A.href url <# title
      , TextNode " "
      ]

navigationSplice :: Splice AppHandler
navigationSplice = do
  request <- getsRequest rqURI
  tags <- lift getTags
  return [H.div <. "nav-collapse topnavbar-collapsible-block" <&
    (H.ul <. "nav topnavbar-collapsible-content"
      <&& createList (T.pack $ normalizeRequest $ unpack request)
      <& (
        H.li <. "themes-box-toggle" <&&
          [ H.a <# "Темы"
          , H.div <. "themes-box" <@ A.style "display: none;"
            <& (H.div <. "themes-button" <# "Темы " <&& createThemesList)
            <&& renderTagsCloud (normalizeTags tags)
          ]
      )
    )]
  where
    normalizeRequest request
      | (last request == '/') && (length request > 1) = init request
      | otherwise = request 

    normalizeTags tags = map (updateWeigth (getWeight minWeight) (getWeight maxWeight)) $ sort tags
      where 
        getWeight (Tag (_, count)) = count
        minWeight = minimumBy weightCompare tags
        maxWeight = maximumBy weightCompare tags
      
        weightCompare (Tag (_, count1)) (Tag (_, count2)) = compare count1 count2

        updateWeigth minW maxW (Tag (tag, count)) = 
          Tag (tag, round 
            ((5 * ((fromIntegral count :: Double) - fromIntegral minW) + 
              fromIntegral maxW - fromIntegral minW) / 
              (fromIntegral maxW - fromIntegral minW)))

    renderTagsCloud tags = 
      [ H.div <. "tags-wrapper" <&& concatMap renderTagsCloud' tags ]
    renderTagsCloud' (Tag (tag, weight)) = 
      [ H.a <. T.append "weight-" (T.pack $ show weight)
        <@ A.href ("/tag/" `T.append` tag)
        <# tag
      , TextNode " "
      ]

revisionSplice :: Splice AppHandler
revisionSplice = 
  return [ TextNode resourcesRevision ]

mobileSplice :: Splice AppHandler
mobileSplice = do
  userAgent <- withRequest (return . T.decodeUtf8 . fromMaybe "" . getHeader "User-Agent")

  return [ TextNode $
    if "Opera Mobi" `T.isInfixOf` userAgent || "Opera Mini" `T.isInfixOf` userAgent ||
        "Android" `T.isInfixOf` userAgent || "Silk" `T.isInfixOf` userAgent || "PlayBook" `T.isInfixOf` userAgent ||
        "iPad" `T.isInfixOf` userAgent || "iPhone" `T.isInfixOf` userAgent || "iPod" `T.isInfixOf` userAgent ||
        (not ("Opera" `T.isPrefixOf` userAgent) && ("WebKit" `T.isInfixOf` userAgent) && ("Mobile" `T.isInfixOf` userAgent))
      then "mobile" else "no-mobile"
    ]

error404 :: AppHandler ()
error404 = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  render "404"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = 
  [ ("/", index)
  , ("/page/:page", index)
  , ("/tag/:tag", index)
  , ("/tag/:tag/page/:page", index)
  , ("/post/:post", showPost)
  , ("/about", aboutMe)
  , ("/shoutbox", shoutbox)
  , ("/archive", archive)
  , ("/latest", latestMovies)
  , ("/rss", rss)
  , ("/sitemap.xml", sitemap)
  , ("/vault", method POST vaultAction)
  , ("/vault", vault)
  , ("/vault/edit", vaultAllowed vaultEdit)
  , ("/vault/edit/:id", vaultAllowed vaultEdit)
  , ("/vault/delete/:id", vaultAllowed vaultDelete)
  , ("/vault/files", vaultAllowed vaultFiles)
  , ("/vault/renderpost", vaultAllowed vaultRenderPost)
  , ("/vault/checkurl", vaultAllowed vaultCheckUrl)
  , ("/vault/fileshandler", vaultAllowed vaultFilesService)
  , ("/vault/fileupload", vaultAllowed vaultFileUpload)
  , ("", serveDirectory "static")
  , ("", error404)
  ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "haskell-blog" "A blog written in Haskell." Nothing $ do
  h <- nestSnaplet "heist" heist $ heistInit' "templates" commonSplices
  let 
    mysqlConnection = connectMySQL connectInfo
  _dblens' <- nestSnaplet "hdbc" dbLens $ hdbcInit mysqlConnection
  _sesslens' <- nestSnaplet "session" sessLens $ initCookieSessionManager
    "config/site_key.txt" "_session" Nothing -- TODO check cookie expiration
  wrapSite (setEncoding *>)
  wrapSite $ withSession sessLens
  addRoutes routes
  return 
    App 
      { _heist = h
      , _dbLens = _dblens'
      , _sessLens = _sesslens'
      }
  where
    commonSplices = bindSplices 
      [ ("navigation", navigationSplice)
      , ("metadata", metadataSplice defaultMetadata)
      , ("revision", revisionSplice)
      , ("mobile", mobileSplice)
      , ("disqusVars", disqusVarsSplice defaultDisqusVars)
      ] 
      defaultHeistState
  
