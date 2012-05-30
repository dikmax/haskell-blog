{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack, append)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Search (replace)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.HDBC.MySQL
import           Prelude hiding (id)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Text.Pandoc
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
import           Text.Blaze.Renderer.XmlHtml
------------------------------------------------------------------------------
import           Application
import           Config
import           Database
import           Site.Rss

-- Utility functions
decodedParam :: MonadSnap f => ByteString -> f ByteString
decodedParam p = fromMaybe "" <$> getParam p
decodedPostParam :: MonadSnap f => ByteString -> f ByteString
decodedPostParam p = fromMaybe "" <$> getPostParam p

------------------------------------------------------------------------------
index :: Handler App App ()
index =  ifTop $ do 
  counter <- with sessLens $ do
    v <- getFromSession "counter"     
    let newV = 1 + maybe 0 (read . T.unpack) v
    setInSession "counter" $ T.pack $ show newV
    return newV

  let 
    indexSplices = 
      [ ("posts", latestPostsSplice)
      , ("counter", counterSplice counter)
      ]
  heistLocal (bindSplices indexSplices) $ render "index"

latestPostsSplice :: Splice AppHandler
latestPostsSplice = do
  posts <- lift getLatestPosts
  return [Element "div" [("class", "posts")] $ map renderPost posts]

counterSplice :: Integer -> Splice AppHandler
counterSplice counter = return [Element "div" [] 
  [TextNode $ T.pack $ "Counter: " ++ show counter]]     

renderPost :: Post -> Node 
renderPost post = 
  Element "div" [("class", "post")] [
    Element "h1" [("class", "post-title")] [
      Element "a" [("href", T.decodeUtf8 $ "/post/" `append` postUrl post)] 
        [TextNode $ T.decodeUtf8 $ postTitle post]
    ],
    Element "div" [("class", "post-body")] $
      renderHtmlNodes $  
        writeHtml defaultWriterOptions $ readMarkdown defaultParserState $ 
          T.unpack $ T.decodeUtf8 $ postText post
  ]

--
-- Show post Action
--
showPost :: Handler App App ()
showPost = do
    postUrl' <- decodedParam "post"
    let showPostSplices = [("post", postSplice postUrl')]
    heistLocal (bindSplices showPostSplices) $ render "post"    

postSplice :: ByteString -> Splice AppHandler
postSplice postUrl' = do
  post <- lift $ getPost postUrl'
  return [renderPost post]
        
--
-- About me action
--

aboutMe :: Handler App App ()
aboutMe = render "about"

--
-- Vault action
--
vault :: AppHandler ()
vault = do
  isAdminLogin <- with sessLens $ getFromSession "isAdminLogin"
  maybe (render "vaultlogin") (\_ -> vaultMain) isAdminLogin

vaultMain :: AppHandler ()
vaultMain = heistLocal (bindSplice "posts" vaultPostsListSplice) $
  render "vault"

vaultPostsListSplice :: Splice AppHandler
vaultPostsListSplice = do
   posts <- lift vaultGetPostsList
   return $ map renderPost' posts
   where 
     renderPost' post = 
       Element "tr" [("data-rowid", T.pack $ show $ postId post)] [
         Element "td" [] [TextNode $ T.pack $ show $ postDate post],
         Element "td" [] [TextNode $ if postPublished post then "+" else ""],
         Element "td" [] [TextNode $ T.decodeUtf8 $ postTitle post],
         Element "td" [("class", "actions")] [
           Element "span" [("class", "action-delete")] []
         ]
       ]

vaultEdit :: AppHandler ()
vaultEdit = do
  request <- getRequest
  id <- decodedParam "id"
  post <- getPost' id
  case rqMethod request of
    POST -> vaultSave
    _ -> heistLocal (bindSplice "vault-form" $ vaultPostForm post) $ 
      render "vaultedit"
  where
    getPost' :: HasHdbc m c s => ByteString -> m Post
    getPost' "" = return newPost
    getPost' id = getPostById id
    
-- TODO there should be a way to simplify this function
vaultSave :: AppHandler ()
vaultSave = do
  id <- decodedPostParam "id"
  title <- decodedPostParam "title"
  text <- decodedPostParam "text"
  url <- decodedPostParam "url"
  date <- decodedPostParam "date"
  published <- decodedPostParam "published"
  special <- decodedPostParam "special"  
  let 
    post = Post 
      { postId = read $ unpack id -- TODO validate
      , postTitle = title
      , postText = B.concat . BL.toChunks $ replace "\r\n" newLine text
      , postDate = read $ unpack date -- TODO check for format errors
      , postUrl = url -- TODO check for duplicate
      , postPublished = published /= ""
      , postSpecial = special /= ""
      , postTags = [] -- TODO tags
      }
  savePost post
  redirect "/vault"
  where
    newLine :: ByteString
    newLine = "\n"

-- TODO digestive functors
vaultPostForm :: Post -> Splice AppHandler
vaultPostForm (Post id title text url date published special _) =
  return 
    [
      Element "form" [("class", "form-horizontal"), ("method", "post")] [
        Element "input" [("type", "hidden"), ("name", "id"), 
          ("value", T.pack $ show id)] [],
        Element "fieldset" [] [
          Element "legend" [] [TextNode "Редактирование записи"],
          inputText "Заголовок" "title" title,
          inputText "Url" "url" url,
          inputText "Дата" "date" $ pack $ show date,
          inputCheckbox "Опубликовано" "published" published,
          inputCheckbox "Специальный" "special" special,
          textarea "Текст" "text" text,
          Element "div" [("class", "form-actions")] [
            Element "button" [("type", "submit"), ("class", "btn btn-primary")] 
              [TextNode "Сохранить"],
            Element "button" [("class", "btn")] [TextNode "Отмена"]
          ]
        ]
      ]
    ]
  where
    inputText :: T.Text -> String -> ByteString -> Node
    inputText fieldLabel name value = field fieldLabel name [
        Element "input" [("type", "text"), ("name", T.pack name), 
          ("class", "input-xxlarge"), ("id", T.pack $ "post-" ++ name), 
          ("value", T.decodeUtf8 value)] []
      ]
    inputCheckbox :: T.Text -> String -> Bool -> Node
    inputCheckbox fieldLabel name value = field fieldLabel name [
        Element "input" 
          ([("type", "checkbox"), ("name", T.pack name), 
            ("id", T.pack $ "post-" ++ name)] ++ 
              [("checked", "checked") | value])  
          []
      ]
    textarea :: T.Text -> String -> ByteString -> Node
    textarea fieldLabel name value = field fieldLabel name [
        Element "textarea" [("name", T.pack name),  
          ("id", T.pack $ "post-" ++ name),
          ("class", "input-xxlarge monospace"), ("rows", "20")] [TextNode $ T.decodeUtf8 value]
      ]
    field :: T.Text -> String -> [Node] -> Node
    field fieldLabel fieldName fieldControl =
      Element "div" [("class", "control-group")] [
        Element "label" [("class", "control-label"), 
          ("for", T.pack $ "post-" ++ fieldName)] [
          TextNode fieldLabel
        ],
        Element "div" [("class", "controls")] fieldControl
      ]

vaultAction :: AppHandler ()
vaultAction = do
  action <- decodedPostParam "action"
  case action of
    "login" -> do
      login <- decodedPostParam "login"
      password <- decodedPostParam "password"
      if (login == adminLogin) && (password == adminPassword)
        then do
          with sessLens $ do 
            setInSession "isAdminLogin" "1"
            commitSession
          redirect "/vault"
        else heistLocal (bindString "error" "Неверные данные") $ render "vaultlogin"

    "logout" -> do
      with sessLens $ do
        deleteFromSession "isAdminLogin"
        commitSession
      redirect "/vault"

    _ -> redirect "/vault"

vaultDelete :: AppHandler ()
vaultDelete = do
  id <- decodedParam "id"
  deletePost id
  redirect "/vault"

vaultAllowed :: AppHandler () -> AppHandler ()
vaultAllowed action = do 
  isAdminLogin <- with sessLens $ getFromSession "isAdminLogin"
  maybe (redirect "/vault") (\_ -> action) isAdminLogin
  
--
-- Navigation
-- 
siteStructure :: [(String, String)]
siteStructure = [("/", "Home"), ("/about", "About me")]

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
      Element "ul" [("class", "nav")] (createList $ normalizeRequest $ unpack request)
    ]]
  where
    normalizeRequest request
      | (last request == '/') && (length request > 1) = init request
      | otherwise = request 

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = 
  [ ("/", index)
  , ("/post/:post", showPost)
  , ("/about", aboutMe)
  , ("/rss", rss)
  , ("/vault", method POST vaultAction)
  , ("/vault", vault)
  , ("/vault/edit", vaultAllowed vaultEdit)
  , ("/vault/edit/:id", vaultAllowed vaultEdit)
  , ("/vault/delete/:id", vaultAllowed vaultDelete)
  , ("", with heist heistServe)
  , ("", serveDirectory "static")
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
    commonSplices = bindSplices [("navigation", navigationSplice)] 
      defaultHeistState
  
