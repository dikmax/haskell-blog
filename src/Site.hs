{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
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
-- About me action
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
  
--
-- Navigation
-- 
siteStructure :: [(String, String)]
siteStructure = 
  [ ("/about", "Обо мне")
  , ("/shoutbox", "Shoutbox")
  ]

createList :: String -> [Node]
createList request = map listItem siteStructure
  where
    listItem item = Element "li" [("class", "active") | request == fst item] [
      Element "a" [("href", T.pack $ fst item)] [
        TextNode $ T.pack (snd item) ]
      ]

navigationSplice :: Splice AppHandler
navigationSplice = do
  request <- getsRequest rqContextPath
  return [Element "div" [("class", "nav-collapse")] [
      Element "ul" [("class", "nav")] $
        createList $ normalizeRequest $ unpack request
    ]]
  where
    normalizeRequest request
      | (last request == '/') && (length request > 1) = init request
      | otherwise = request 

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
  , ("/rss", rss)
  , ("/vault", method POST vaultAction)
  , ("/vault", vault)
  , ("/vault/edit", vaultAllowed vaultEdit)
  , ("/vault/edit/:id", vaultAllowed vaultEdit)
  , ("/vault/delete/:id", vaultAllowed vaultDelete)
  , ("/vault/files", vaultAllowed vaultFiles)
  , ("/vault/renderpost", vaultAllowed vaultRenderPost)
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
      ] 
      defaultHeistState
  
