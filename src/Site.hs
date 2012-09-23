{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.List (maximumBy, minimumBy, sort)
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
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
import           Text.XmlHtml.Cursor
------------------------------------------------------------------------------
import           Application
import           Config
import           Database
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
    pageNum = case reads $ unpack page of
      [(x, "")] -> x
      _ -> 1
    indexSplices = 
      [ ("posts", postsSplice pageNum tag) 
      , ("pagination", paginationSplice pageNum tag)
      , ("metadata", metadataSplice $ defaultMetadata 
        { metaUrl = maybe "" (\t -> "/tag/" `T.append` T.decodeUtf8 t) tag
            `T.append` if pageNum == 1 then "" else "/page/" `T.append` (T.pack $ show pageNum)
        , metaType = FacebookBlog
        })
      ]
  heistLocal (bindSplices indexSplices) $ render "index"

postsSplice :: Int -> Maybe ByteString -> Splice AppHandler
postsSplice page tag = do
  posts <- lift $ getPosts tag ((page - 1) * postsPerPage) postsPerPage
  return [Element "div" [("class", "posts")] $ map renderPostInList posts]

paginationSplice :: Int -> Maybe ByteString -> Splice AppHandler
paginationSplice page tag = do
  postsCount <- lift $ getPostsCount tag
  let
    prevDisabled = page * postsPerPage >= postsCount  
    
    prevLink :: Text
    prevLink = maybe "" (\t -> "/tag/" `T.append` T.decodeUtf8 t) tag 
      `T.append` "/page/" `T.append` (T.pack $ show (page + 1))
    
    nextDisabled = page <= 1

    nextLink :: Text
    nextLink = maybe "" (\t -> "/tag/" `T.append` T.decodeUtf8 t) tag
      `T.append`  if page == 2 then "" else "/page/" `T.append` 
        (T.pack $ show (page - 1))

    prevElement = if prevDisabled
      then []
      else [Element "li" [("class", "previous")]
        [ Element "a" [("href", prevLink)] [TextNode "← Старше"] ] ]
    nextElement = if nextDisabled      
      then []
      else [Element "li" [("class", "next")]
        [ Element "a" [("href", nextLink)] [TextNode "Моложе →"] ] ]
  return [Element "ul" [("class", "pager")] $ prevElement ++ nextElement ]
        
renderPostInList :: Post -> Node 
renderPostInList post = 
  Element "article" 
    [ ("class", "post component-panel")
    , ("itemprop", "blogPost")
    , ("itemscope", "itemscope")
    , ("itemtype", "http://schema.org/BlogPosting")
    ] [
    Element "div" [("itemprop", "author"), ("itemscope", "itemscope"), ("itemtype", "http://schema.org/Person")] 
      [ Element "meta" [("itemprop", "name"), ("content", "Maxim Dikun")] []
      , Element "link" [("itemprop", "url"), ("content", "http://dikmax.name/about")] []
      , Element "link" [("itemprop", "url"), ("content", "https://plus.google.com/109129288587536990618/posts")] []
      ],
    Element "h1" [("class", "post-title"), ("itemprop", "name")] [
      Element "a" [("href", "/post/" `T.append` postUrl post), ("itemprop", "url")] 
        [TextNode $ postTitle post]
    ],
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
      , TextNode " | "
      , Element "i" [("class" , "icon-comment")] []
      , TextNode " "
      , Element "span" [("class", "post-comments")]
        [ Element "a"
          [ ("href", "/post/" `T.append`
            postUrl post `T.append` "#disqus_thread" )
          , ("itemprop", "discussionUrl")
          ]
          [ TextNode "Считаем комментарии..." ]
        ]
      ]) . lastChild . fromNode

-- |
-- Show post Action
--
showPost :: AppHandler ()
showPost = do
  url <- decodedParam "post"
  post <- getPost url

  maybe error404 
    (\p -> heistLocal (bindSplices 
      [ ("post", return [renderResult p])
      , ("metadata", metadataSplice $ defaultMetadata 
        { metaTitle = Just $ postTitle p
        , metaUrl = "/post/" `T.append` (T.decodeUtf8 $ url) 
        , metaType = FacebookArticle (postDate p) (postTags p) (getImage $ renderResult p)
        , metaDescription = getDescription $ renderResult p
        })
      ]) $ render "post") post
  where
    renderResult post = renderSinglePost post

    emptyDescription = "Мой персональный блог"

    -- filter .post-tags
    getDescription :: Node -> Text
    getDescription = maybe emptyDescription 
      (until (not . T.null) (\_ -> emptyDescription) . getDescription') . 
      findChild (checkMainDiv . current) . fromNode
    checkMainDiv node = (maybe False (== "div") $ tagName node) &&
      (maybe False (== "articleBody") $ getAttribute "itemprop" node)

    getDescription' :: Cursor -> Text
    getDescription' = cutDescription . transformDescription .
      T.intercalate " " . map nodeText . filter checkParagraph .
      maybe [] siblings . firstChild
    checkParagraph = maybe False (`elem` ["p", "h2", "h3", "h4", "h5", "h6"]) . tagName

    transformDescription = T.replace "\n" " "
    cutDescription d
      | T.length d > 512 = (T.stripEnd $ fst $ T.breakOnEnd " " $ T.take 512 d) `T.append` "..."
      | otherwise = d

    getImage :: Node -> Maybe Text
    getImage = 
      maybe Nothing (        
        maybe Nothing (getAttribute "src" . current) . 
        findChild (maybe False (== "img") . tagName . current)
      ) . findRec (checkFigure . current) . fromNode      
    checkFigure node = (maybe False (== "div") $ tagName node) &&
      (maybe False (== "figure") $ getAttribute "class" node)

-- |
-- About me action
--
aboutMe :: AppHandler ()
aboutMe = heistLocal (bindSplices
  [ ("about", aboutSplice)
  , ("metadata", metadataSplice $ defaultMetadata 
    { metaTitle = Just "Обо мне"
    , metaUrl = "/about"
    , metaType = FacebookProfile
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
    , metaUrl = "/shoutbox"
    })
  ] ) $ render "shoutbox"
  
shoutboxSplice :: Splice AppHandler
shoutboxSplice = do
  post <- lift $ getPost "shoutbox"
  return $ maybe [] (\p -> [renderPostBody p "mainContentOfPage"]) post

-- |
-- Latest movies action
--
latestMovies :: AppHandler ()
latestMovies = heistLocal (bindSplices
  [ ("latest", latestMoviesSplice)
  , ("metadata", metadataSplice $ defaultMetadata 
    { metaTitle = Just "Последние просмотренные фильмы"
    , metaUrl = "/latest"
    })
  ] ) $ render "latest"
  
latestMoviesSplice :: Splice AppHandler
latestMoviesSplice = do
  post <- lift $ getPost "latest"
  return $ maybe [] (\p -> [renderPostBody p "mainContentOfPage"]) post
  
--
-- Navigation
-- 
siteStructure :: [(Text, Text)]
siteStructure = 
  [ ("Обо мне", "/about")
  , ("Shoutbox", "/shoutbox")
  , ("Фильмы", "/latest")
  ]

createList :: Text -> [Node]
createList _ = map listItem siteStructure
  where
    listItem (title, url) = Element "li" [] [
      Element "a" [("href", url)] [
        TextNode title ]
      ]

themesList :: [(Text, Text)]
themesList = 
  [ ]

createThemesList :: [Node]
createThemesList = concatMap listItem themesList
  where
    listItem :: (Text, Text) -> [Node]
    listItem (title, url) = 
      [ Element "a" [("href", url), ("class", "main-theme-link")] 
        [TextNode title]
      , TextNode " "
      ]

navigationSplice :: Splice AppHandler
navigationSplice = do
  request <- getsRequest rqURI
  tags <- lift getTags
  return [Element "div" [("class", "nav-collapse topnavbar-collapsible-block")] [
      Element "ul" [("class", "nav topnavbar-collapsible-content")] $
        (createList $ T.pack $ normalizeRequest $ unpack request) ++
        [ Element "li" [("class", "themes-box-toggle")]
          [ Element "a" [] [TextNode "Темы"]
          , Element "div" [("class", "themes-box"), ("style", "display: none;")] $
            [Element "div" [("class", "themes-button")] $ [TextNode "Темы "] ++ createThemesList ]
               ++ (renderTagsCloud $ normalizeTags tags)
          ]
        ]
    ]]
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
      [ Element "div" [("class", "tags-wrapper")] $ concatMap renderTagsCloud' tags ]
    renderTagsCloud' (Tag (tag, weight)) = 
      [ Element "a" 
        [ ("href", "/tag/" `T.append` tag)
        , ("class", T.append "weight-" $ T.pack $ show weight)
        ] 
        [TextNode tag]
      , TextNode " "
      ]

revisionSplice :: Splice AppHandler
revisionSplice = 
  return [ TextNode resourcesRevision ]

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
    mysqlConnection = connectMySQL $ 
      MySQLConnectInfo "127.0.0.1" "root" "" "haskellblog" 3306 "" Nothing
  _dblens' <- nestSnaplet "hdbc" dbLens $ hdbcInit mysqlConnection
  _sesslens' <- nestSnaplet "session" sessLens $ initCookieSessionManager
    "config/site_key.txt" "_session" Nothing -- TODO check cookie expiration
  wrapHandlers (setEncoding *>)
  wrapHandlers $ withSession sessLens
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
      ] 
      defaultHeistState
  
