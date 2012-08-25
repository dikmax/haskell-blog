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
import           Prelude hiding (id)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
------------------------------------------------------------------------------
import           Application
import           Config
import           Database
import           Site.Common
import           Site.Rss
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
  Element "div" [("class", "post component-panel")] [
    Element "p" [("class", "post-date")] 
      [TextNode $ T.pack $ formatTime timeLocale "%A, %e %B %Y, %R." $ 
        postDate post],
    Element "h1" [("class", "post-title")] [
      Element "a" [("href", "/post/" `T.append` postUrl post)] 
        [TextNode $ postTitle post]
    ],
    renderPostBody post,
    Element "p" [("class", "post-comments")] [
      Element "a" [("href", "/post/" `T.append` 
        postUrl post `T.append` "#disqus_thread" )] 
        [TextNode "Считаем комментарии..."]
    ]
  ]

-- |
-- Show post Action
--
showPost :: AppHandler ()
showPost = do
  url <- decodedParam "post"
  post <- getPost url
  maybe error404 
    (\p -> heistLocal (bindSplices 
      [ ("post", postSplice p)
      , ("page-title", pageTitleSplice $ Just $ postTitle p)
      ]) $ render "post") post

postSplice :: Post -> Splice AppHandler
postSplice post = return [renderSinglePost post] 
        
-- |
-- About me action
--
aboutMe :: AppHandler ()
aboutMe = heistLocal (bindSplices
  [ ("about", aboutSplice)
  , ("page-title", pageTitleSplice $ Just "Обо мне")
  ] ) $ render "about"
  
aboutSplice :: Splice AppHandler
aboutSplice = do
  post <- lift $ getPost "about"
  return $ maybe [] (\p -> [renderPostBody p]) post

-- |
-- Shoutbox action
--
shoutbox :: AppHandler ()
shoutbox = heistLocal (bindSplices
  [ ("shoutbox", shoutboxSplice)
  , ("page-title", pageTitleSplice $ Just "Shoutbox")
  ] ) $ render "shoutbox"
  
shoutboxSplice :: Splice AppHandler
shoutboxSplice = do
  post <- lift $ getPost "shoutbox"
  return $ maybe [] (\p -> [renderPostBody p]) post

-- |
-- Latest movies action
--
latestMovies :: AppHandler ()
latestMovies = heistLocal (bindSplices
  [ ("latest", latestMoviesSplice)
  , ("page-title", pageTitleSplice $ Just "Последние просмотренные фильмы")
  ] ) $ render "latest"
  
latestMoviesSplice :: Splice AppHandler
latestMoviesSplice = do
  post <- lift $ getPost "latest"
  return $ maybe [] (\p -> [renderPostBody p]) post
  
--
-- Navigation
-- 
siteStructure :: [(Text, Text)]
siteStructure = 
  [ ("Обо мне", "/about")
  , ("Shoutbox", "/shoutbox")
  ]

createList :: String -> [Node]
createList request = map listItem siteStructure
  where
    listItem (title, url) = Element "li" [("class", "active") | request == T.unpack url] [
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
  request <- getsRequest rqContextPath
  tags <- lift getTags
  return [Element "div" [("class", "nav-collapse")] [
      Element "ul" [("class", "nav")] $
        (createList $ normalizeRequest $ unpack request) ++
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


pageTitleSplice :: Maybe Text -> Splice AppHandler
pageTitleSplice Nothing = 
  return 
    [ Element "title" [] 
      [ TextNode "[dikmax's blog]" ]
    ]
pageTitleSplice (Just t) = 
  return 
    [ Element "title" [] 
      [ TextNode $ t `T.append` " :: [dikmax's blog]" ]
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
      , ("page-title", pageTitleSplice Nothing)
      , ("revision", revisionSplice)
      ] 
      defaultHeistState
  
