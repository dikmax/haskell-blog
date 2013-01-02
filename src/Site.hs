{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import qualified Data.HashMap.Strict as Map
import           Data.List (groupBy, maximumBy, minimumBy, sort)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Database.HDBC.MySQL
-- import           Heist
import qualified Heist.Interpreted as I
-- import qualified Heist.Compiled as C
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           System.Locale
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

-- | Main page handler
index :: Handler App App ()
index =  ifTop $ do
  page <- decodedParam "page"
  tag <- getParam "tag"
  let
    tagText = T.decodeUtf8' $ fromMaybe "" tag
    tagLink
      | isNothing tag = Nothing
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
      , ("rss", rssSplice tag)
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
    (\_ -> heistLocal (I.bindSplices indexSplices) $ render "index")
    tagText

-- | Posts splice
postsSplice :: Int                 -- ^ page number
            -> Maybe ByteString    -- ^ tag
            -> I.Splice AppHandler
postsSplice page tag = do
  posts <- lift $ getPosts tag ((page - 1) * postsPerPage) postsPerPage
  return [H.div <. "posts" <&& map renderPostInList posts]

-- | Splice with pagination
paginationSplice :: Int                 -- ^ page number
                 -> Maybe ByteString    -- ^ tag
                 -> I.Splice AppHandler
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

-- | Render post in list on main page
renderPostInList :: Post    -- ^ post to render
                 -> Node
renderPostInList post = 
  H.article
    <. "post component-panel"
    <@ A.itemprop "blogPost"
    <@ A.itemscope
    <@ A.itemtype "http://schema.org/BlogPosting"
    <&&
    [
      H.div <@ A.itemprop "author" <@ A.itemscope <@ A.itemtype "http://schema.org/Person" <&&
        [ H.meta <@ A.itemprop "name" <@ A.content "Maxim Dikun"
        , H.link <@ A.itemprop "url" <@ A.href "http://dikmax.name/about"
        , H.link <@ A.itemprop "url" <@ A.href "https://plus.google.com/109129288587536990618/posts"
        ]
    , H.h1 <. "post-title" <@ A.itemprop "name" <& (
        H.a <@ A.href ("/post/" `T.append` postUrl post) <@ A.itemprop "url"
          <# postTitle post
      )
    , H.meta <@ A.itemprop "dateCreated" <@ A.content (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" $ postDate post)
    ] ++ addCommentsBlock post True (postDate post) (renderPostBody post "articleBody")

-- | Single post hanlder
showPost :: AppHandler ()
showPost = do
  url <- decodedParam "post"
  post <- getPost url
  comments <- getComments post
  isAdminLogin <- with sessLens $ getFromSession "isAdminLogin"
  maybe error404 
    (\p -> heistLocal (I.bindSplices
      [ ("post", return [renderSinglePost (maybe False (\_ -> True) isAdminLogin) p])
      , ("comments", commentsSplice comments)
      , ("metadata", metadataSplice $ defaultMetadata 
        { metaTitle = Just $ postTitle p
        , metaUrl = "/post/" `T.append` T.decodeUtf8 url
        , metaType = FacebookArticle (postDate p) (postTags p) (getImage $ renderSinglePost False p)
        , metaDescription = getDescription $ renderSinglePost False p
        })
      , ("disqusVars", disqusVarsSplice $ defaultDisqusVars
        { disqusIdentifier = Just $ T.decodeUtf8 url
        , disqusUrl = Just $ "http://dikmax.name/post/" `T.append` T.decodeUtf8 url
        , disqusTitle = Just $ postTitle p
        })
      ]) $ render "post") post
  where
    -- Text for empty description (will be shown in case of error)
    emptyDescription = "Мой персональный блог"

    -- Looking for div with article body and then extracting text from it
    getDescription :: Node -> Text
    getDescription = maybe emptyDescription 
      (until (not . T.null) (const emptyDescription) . getDescription') .
      findChild (checkMainDiv . current) . fromNode

    -- Check if element is main div
    checkMainDiv node = maybe False (== "div") (tagName node) &&
      maybe False (== "articleBody") (getAttribute "itemprop" node)

    -- Filter node children and extracting text from rest
    getDescription' :: Cursor -> Text
    getDescription' = cutDescription . transformDescription .
      T.intercalate " " . map extractNodeText . filter checkParagraph .
      maybe [] siblings . firstChild

    -- Behaves link nodeText except that is skips footnote links
    extractNodeText :: Node -> Text
    extractNodeText (TextNode t)    = t
    extractNodeText (Comment _)     = ""
    extractNodeText (Element n cl c)
      | n == "sup" && ("class", "note-link") `elem` cl = ""
      | otherwise = T.concat (map extractNodeText c)

    -- Filter predicate for getDescription'
    checkParagraph = maybe False (`elem` ["p", "h2", "h3", "h4", "h5", "h6"]) . tagName

    -- Replate newlines with spaces
    transformDescription = T.replace "\n" " "

    -- Cut long descriptions
    cutDescription d
      | T.length d > 512 = T.stripEnd (fst $ T.breakOnEnd " " $ T.take 512 d) `T.append` "..."
      | otherwise = d

    -- Looking for images
    getImage :: Node -> Maybe Text
    getImage = 
      maybe Nothing (        
        maybe Nothing (getAttribute "src" . current) . 
        findChild (maybe False (== "img") . tagName . current)
      ) . findRec (checkFigure . current) . fromNode

    -- Check if image is suitable
    checkFigure node = maybe False (== "div") (tagName node) &&
      maybe False (== "figure") (getAttribute "class" node)


-- | Render comments
renderComments :: [PostComment] -- ^ list of comments
               -> [Node]
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
                    else H.a <@ A.href (commentAuthorUrl comment) <# commentAuthorName comment
                  ]
                , H.span <. "post-comment-bullet" <@ ("aria-hidden", "true") <# "•"
                , H.span <. "post-comment-date" <@ A.itemprop "commentTime"
                  <@ A.datetime (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M:%S" $ commentDate comment)
                  <# T.pack (formatTime timeLocale "%A, %e %B %Y, %R" $ commentDate comment)
                ]
            ) <& (H.div <@ A.itemprop "commentText" <&& toComment (parseHTML "comment.html" $ T.encodeUtf8 $ commentBody comment))
        , H.div <. "clearfix"
        ]
    toComment :: Either String Document -> [Node]
    toComment (Left _) = []
    toComment (Right (XmlDocument _ _ nodes)) = nodes
    toComment (Right (HtmlDocument _ _ nodes)) = nodes

-- | Splice for render comments
commentsSplice :: [PostComment]      -- ^ list of comments
               -> I.Splice AppHandler
commentsSplice = return . renderComments

tagsCloud :: AppHandler ()
tagsCloud = heistLocal (I.bindSplices
  [ ("tags", tagsSplice)
  , ("metadata", metadataSplice $ defaultMetadata
    { metaTitle = Just "Темы"
    , metaDescription = "Полный список тем (тегов) на сайте"
    , metaUrl = "/tags"
    })
  , ("disqusVars", disqusVarsSplice $ defaultDisqusVars
    { disqusIdentifier = Just "tags"
    , disqusUrl = Just  "http://dikmax.name/tags"
    , disqusTitle = Just "Темы"
    })
  ]) $ render "tags"

tagsSplice :: I.Splice AppHandler
tagsSplice = do
  tags <- lift $ getTags 0
  return $ renderTagsCloud tags

-- | About page
aboutMe :: AppHandler ()
aboutMe = heistLocal (I.bindSplices
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

-- | About splice
aboutSplice :: I.Splice AppHandler
aboutSplice = do
  post <- lift $ getPost "about"
  return $ maybe [] (\p -> [renderPostBody p "mainContentOfPage"]) post

-- | Shoutbox page
shoutbox :: AppHandler ()
shoutbox = heistLocal (I.bindSplices
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

-- | Shoutbox splice
shoutboxSplice :: I.Splice AppHandler
shoutboxSplice = do
  post <- lift $ getPost "shoutbox"
  comments <- lift $ getComments post
  return $ maybe [] (\p ->
    [ renderPostBody p "mainContentOfPage"
    , H.div <@ ("id", "disqus_thread") <&& renderComments comments
    ]) post

-- | Archive action
archive :: AppHandler ()
archive = heistLocal (I.bindSplices
  [ ("archive", archiveSplice)
  , ("metadata", metadataSplice $ defaultMetadata
    { metaTitle = Just "Архив"
    , metaDescription = "Список всех постов для \"быстрого поиска\""
    , metaUrl = "/archive"
    })
  ] ) $ render "archive"

-- | Archive splice
archiveSplice :: I.Splice AppHandler
archiveSplice = do
  posts <- lift $ getPosts Nothing 0 1000

  return [H.ul <. "media-list" <&& renderList posts]
  where
    -- Render posts list
    renderList posts =
      concatMap (\list -> (H.h2 <. "archive-month" <# T.pack (formatTime archiveLocale "%B %Y" $ postDate $ head list)) : map renderPost list) $
        groupBy (\a b -> isEqual (toGregorian $ localDay $ postDate a) (toGregorian $ localDay $ postDate b)) posts

    -- Right month names
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

    -- Is same month
    isEqual (y1, m1, _) (y2, m2, _) = y1 == y2 && m1 == m2

    -- Extract day from date
    getDay (_, _, d) = T.pack $ show d

    -- Render single post
    renderPost post =
      H.li <. "media" <@ A.itemtype "http://schema.org/BlogPosting" <@ A.itemscope <@ A.itemprop "blogPost" <&&
      [ H.div <@ A.itemprop "author" <@ A.itemscope <@ A.itemtype "http://schema.org/Person" <&&
        [ H.meta <@ A.itemprop "name" <@ A.content "Maxim Dikun"
        , H.link <@ A.itemprop "url" <@ A.href "http://dikmax.name/about"
        , H.link <@ A.itemprop "url" <@ A.href "https://plus.google.com/109129288587536990618/posts"
        ]
      , H.meta <@ A.itemprop "dateCreated" <@ A.content (T.pack $ formatTime timeLocale "%Y-%m-%dT%H:%M" $ postDate post)
      , H.span <. "pull-left"  <&
        (
          H.span <. "media-object archive-day" <# getDay (toGregorian $ localDay $ postDate post)
        )
      , H.h4 <. "media-heading" <@ A.itemprop "name" <&
        (
          H.a <@ A.itemprop "url" <@ A.href ("/post/" `T.append` postUrl post) <# postTitle post
        )
      , H.div <&& renderTags (postTags post)
      ]

-- | Latest movies handler
latestMovies :: AppHandler ()
latestMovies = heistLocal (I.bindSplices
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

-- | Latest movies splice
latestMoviesSplice :: I.Splice AppHandler
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

-- | Defines man site structure
siteStructure :: [(Text, Text)]
siteStructure = 
  [ ("Обо мне", "/about")
  , ("Shoutbox", "/shoutbox")
  , ("Архив", "/archive")
  , ("Фильмы", "/latest")
  ]

-- | Create navigation nodes
createList :: Text   -- ^ request url TODO
           -> [Node]
createList _ = map listItem siteStructure
  where
    listItem (title, url) = H.li <& H.a <@ A.href url <# title

-- | Themes (main tags) not yet used
themesList :: [(Text, Text)]
themesList = 
  [ ]

-- | Transform themes list to html-nodes
createThemesList :: [Node]
createThemesList = concatMap listItem themesList
  where
    listItem :: (Text, Text) -> [Node]
    listItem (title, url) = 
      [ H.a <. "main-theme-link" <@ A.href url <# title
      , TextNode " "
      ]

-- | Splice for rendering navbar
navigationSplice :: I.Splice AppHandler
navigationSplice = do
  request <- getsRequest rqURI
  tags <- lift $ getTags 2
  return [H.div <. "nav-collapse topnavbar-collapsible-block" <&
    (H.ul <. "nav topnavbar-collapsible-content"
      <&& createList (T.pack $ normalizeRequest $ unpack request)
      <& (
        H.li <. "themes-box-toggle" <&&
          [ H.a <# "Темы"
          , H.div <. "themes-box" <@ A.style "display: none;"
            <& (H.div <. "themes-button" <# "Темы " <&& createThemesList)
            <&&
              ( renderTagsCloud tags )
            <&
              ( H.div <. "tags-more" <&
                ( H.a <@ A.href "/tags" <# "Огласите весь список, пожалуйста..." )
              )
          ]
      )
    )]
  where
    normalizeRequest request
      | (last request == '/') && (length request > 1) = init request
      | otherwise = request 

-- Render tags cloud from list
renderTagsCloud :: [Tag] -> [Node]
renderTagsCloud tags =
  [ H.div <. "tags-wrapper" <&& concatMap renderTagsCloud_ normalizeTags ]
  where
    normalizeTags = map (updateWeight (getWeight minWeight) (getWeight maxWeight)) $ sort tags
      where
        getWeight (Tag (_, count)) = count
        minWeight = minimumBy weightCompare tags
        maxWeight = maximumBy weightCompare tags

        weightCompare (Tag (_, count1)) (Tag (_, count2)) = compare count1 count2

        updateWeight :: Int -> Int -> Tag -> (Text, Int, Text)
        updateWeight minW maxW (Tag (tag, count)) =
          ( tag
          , round
            ((5 * ((fromIntegral count :: Double) - fromIntegral minW) +
              fromIntegral maxW - fromIntegral minW) /
              (fromIntegral maxW - fromIntegral minW))
          , countText count)

        countText :: Int -> Text
        countText count
          | count `mod` 100 `div` 10 == 1 =
            T.pack (show count) `T.append` " постов"
          | count `mod` 10 == 1 =
            T.pack (show count) `T.append` " пост"
          | count `mod` 10 == 2 || count `mod` 10 == 3 || count `mod` 10 == 4 =
            T.pack (show count) `T.append` " поста"
          | otherwise =
            T.pack (show count) `T.append` " постов"

    renderTagsCloud_ (tag, weight, count) =
      [ H.a <. T.append "weight-" (T.pack $ show weight)
        <@ A.href ("/tag/" `T.append` tag)
        <@ A.title count
        <# tag
      , TextNode " "
      ]

-- | Splice for rendering current resources revision
revisionSplice :: I.Splice AppHandler
revisionSplice = 
  return [ TextNode resourcesRevision ]

-- | Splice to detect is userAgent is mobile
mobileSplice :: I.Splice AppHandler
mobileSplice = do
  userAgent <- withRequest (return . T.decodeUtf8 . fromMaybe "" . getHeader "User-Agent")

  return [ TextNode $
    if "Opera Mobi" `T.isInfixOf` userAgent || "Opera Mini" `T.isInfixOf` userAgent ||
        "Android" `T.isInfixOf` userAgent || "Silk" `T.isInfixOf` userAgent || "PlayBook" `T.isInfixOf` userAgent ||
        "iPad" `T.isInfixOf` userAgent || "iPhone" `T.isInfixOf` userAgent || "iPod" `T.isInfixOf` userAgent ||
        (not ("Opera" `T.isPrefixOf` userAgent) && ("WebKit" `T.isInfixOf` userAgent) && ("Mobile" `T.isInfixOf` userAgent))
      then "mobile" else "no-mobile"
    ]

-- | Splice to show years in copyright
copyrightYearSplice :: Integer -> I.Splice AppHandler
copyrightYearSplice startYear = do
  currentTime <- liftIO getCurrentTime
  return
    [
      H.span <@ A.itemprop "copyrightYear" <#
        (
          if (startYear == getYear currentTime)
            then
              T.pack (show startYear)
            else
              T.pack (show startYear) `T.append`
              " — " `T.append`
              T.pack (show $ getYear currentTime)
        )
    ]
  where
    getYear time = getYear_ $ toGregorian $ localDay $ utcToLocalTime (minutesToTimeZone 180) time
    getYear_ (year, _, _) = year

-- | Handler for sending 404 error
error404 :: AppHandler ()
error404 = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  render "404"

staticMimeMap :: MimeMap
staticMimeMap = Map.fromList
  [ ( ".css"     , "text/css"                          )
  , ( ".dtd"     , "text/xml"                          )
  , ( ".eot"     , "application/vnd.ms-fontobject"     )
  , ( ".gif"     , "image/gif"                         )
  , ( ".jpeg"    , "image/jpeg"                        )
  , ( ".jpg"     , "image/jpeg"                        )
  , ( ".js"      , "text/javascript"                   )
  , ( ".json"    , "application/json"                  )
  , ( ".ico"     , "image/vnd.microsoft.icon"          )
  , ( ".less"    , "text/css"                          )
  , ( ".png"     , "image/png"                         )
  , ( ".svg"     , "image/svg+xml"                     )
  , ( ".ttf"     , "application/x-font-truetype"       )
  , ( ".woff"    , "applicaton/font-woff"              )
  , ( ".xml"     , "text/xml"                          )
  ]

staticDirectoryConfig :: DirectoryConfig (Handler App App)
staticDirectoryConfig = simpleDirectoryConfig
  { mimeTypes = staticMimeMap }
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = 
  [ ("/", index)
  , ("/page/:page", index)
  , ("/tag/:tag", index)
  , ("/tag/:tag/page/:page", index)
  , ("/post/:post", showPost)
  , ("/tags", tagsCloud)
  , ("/about", aboutMe)
  , ("/shoutbox", shoutbox)
  , ("/archive", archive)
  , ("/latest", latestMovies)
  , ("/rss", rss)
  , ("/rss/tag/:tag", rss)
  , ("/sitemap.xml", sitemap)
  , ("/vault", method POST vaultAction)
  , ("/vault", vault)
  , ("/vault/edit", vaultAllowed vaultEdit)
  , ("/vault/edit/:id", vaultAllowed vaultEdit)
  , ("/vault/delete/:id", vaultAllowed vaultDelete)
  , ("/vault/files", vaultAllowed vaultFiles)
  , ("/vault/renderpost", vaultAllowed vaultRenderPost)
  , ("/vault/checkurl", vaultAllowed vaultCheckUrl)
  , ("/vault/postslist", vaultAllowed vaultPostsList)
  , ("/vault/fileshandler", vaultAllowed vaultFilesService)
  , ("/vault/fileupload", vaultAllowed vaultFileUpload)
  , ("", serveDirectoryWith staticDirectoryConfig "static")
  , ("", error404)
  ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "haskell-blog" "A blog written in Haskell." Nothing $ do
  h <- nestSnaplet "heist" heist $ heistInit "templates"
  let 
    mysqlConnection = connectMySQL connectInfo
  _dblens' <- nestSnaplet "hdbc" dbLens $ hdbcInit mysqlConnection
  _sesslens' <- nestSnaplet "session" sessLens $ initCookieSessionManager
    "config/site_key.txt" "_session" Nothing -- TODO check cookie expiration
  addSplices commonSplices
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
    commonSplices =
      [ ("navigation", navigationSplice)
      , ("metadata", metadataSplice defaultMetadata)
      , ("revision", revisionSplice)
      , ("mobile", mobileSplice)
      , ("disqusVars", disqusVarsSplice defaultDisqusVars)
      , ("rss", rssSplice Nothing)
      , ("copyrightYear", copyrightYearSplice 2012)
      ] 

