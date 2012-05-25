{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
--   site. The 'app' function is the initializer that combines everything
--   together and is exported by this module.
--
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Lens.Common (Lens)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Database.HDBC.MySQL
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
import           Database
import           Config

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App App ()
index =  ifTop $ do 
  counter <- with sessLens $ do
    v <- getFromSession "counter"     
    let newV = 1 + maybe 0 (read . T.unpack) v
    setInSession "counter" $ T.pack $ show newV
    return newV
    
  let indexSplices = [("posts", latestPostsSplice), ("counter", counterSplice counter)] 
  heistLocal (bindSplices indexSplices) $ render "index"

latestPostsSplice :: Splice AppHandler
latestPostsSplice = do
   posts <- lift getLatestPosts
   return [Element "div" [("class", "posts")] $ map renderPost posts]
   
counterSplice :: Integer -> Splice AppHandler
counterSplice counter = return [Element "div" [] [TextNode $ T.pack $ "Counter: " ++ show counter]]     

renderPost :: Post -> Node 
renderPost post = 
  Element "div" [("class", "post")] [
    Element "h1" [("class", "post-title")] [
      Element "a" [("href", T.pack $ "/post/" ++ postUrl post)] [TextNode $ T.pack $ postTitle post]
    ],
    Element "div" [("class", "post-body")] $
      renderHtmlNodes $  
        writeHtml defaultWriterOptions $ readMarkdown defaultParserState $ postText post
  ]

--
-- Show post Action
--
showPost :: Handler App App ()
showPost = do
    postUrl <- decodedParam "post"
    let showPostSplices = [("post", postSplice $ unpack postUrl)]
    heistLocal (bindSplices showPostSplices) $ render "post"    
  where
    decodedParam p = fromMaybe "" <$> getParam p
    

postSplice :: String -> Splice AppHandler
postSplice postUrl = do
  post <- lift $ getPost postUrl
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
vaultMain = heistLocal (bindSplice "posts" vaultPostsListSplice) $ render "vault"

vaultPostsListSplice :: Splice AppHandler
vaultPostsListSplice = do
   posts <- lift vaultGetPostsList
   return $ map renderPost posts
   where 
     renderPost post = 
       Element "tr" [] [
         Element "td" [] [TextNode $ T.pack $ show $ postDate post],
         Element "td" [] [TextNode $ if postPublished post then "+" else ""],
         Element "td" [] [TextNode $ T.pack $ postTitle post]
       ]
  
vaultEdit :: AppHandler ()
vaultEdit = heistLocal (bindSplice "vault-form" $ vaultPostForm newPost) $ render "vaultedit"

-- TODO digestive functors
vaultPostForm :: Post -> Splice AppHandler
vaultPostForm post =
  return 
    [
      Element "form" [("class", "form-horizontal"), ("method", "post")] [
        Element "input" [("type", "hidden"), ("name", "action"), ("value", "save")] [],
        Element "fieldset" [] [
          Element "legend" [] [TextNode "Редактирование записи"],
          inputText "Заголовок" "title" $ postTitle post,
          inputText "Url" "url" $ postUrl post,
          inputText "Дата" "date" $ show $ postDate post,
          inputCheckbox "Опубликовано" "published" $ postPublished post,
          inputCheckbox "Специальный" "special" $ postSpecial post,
          textarea "Текст" "text" $ postText post,
          Element "div" [("class", "form-actions")] [
            Element "button" [("type", "submit"), ("class", "btn btn-primary")] [TextNode "Сохранить"],
            Element "button" [("class", "btn")] [TextNode "Отмена"]
          ]
        ]
      ]
    ]
  where
    inputText :: T.Text -> String -> String -> Node
    inputText fieldLabel name value = field fieldLabel name [
        Element "input" [("type", "text"), ("name", T.pack name), 
          ("class", "input-xxlarge"), ("id", T.pack $ "post-" ++ name), ("value", T.pack value)] []
      ]
    inputCheckbox :: T.Text -> String -> Bool -> Node
    inputCheckbox fieldLabel name value = field fieldLabel name [
        Element "input" 
          ([("type", "checkbox"), ("name", T.pack name), 
            ("id", T.pack $ "post-" ++ name)] ++ [("checked", "checked") | value])  
          []
      ]
    textarea :: T.Text -> String -> String -> Node
    textarea fieldLabel name value = field fieldLabel name [
        Element "textarea" [("name", T.pack name),  ("id", T.pack $ "post-" ++ name),
          ("class", "input-xxlarge"), ("rows", "20")] [TextNode $ T.pack value]
      ]
    field :: T.Text -> String -> [Node] -> Node
    field fieldLabel fieldName fieldControl =
      Element "div" [("class", "control-group")] [
        Element "label" [("class", "control-label"), ("for", T.pack $ "post-" ++ fieldName)] [
          TextNode fieldLabel
        ],
        Element "div" [("class", "controls")] fieldControl
      ]
  
vaultAction :: AppHandler ()
vaultAction = do
    action <- decodedParam "action"
    case action of
      "login" -> do
        login <- decodedParam "login"
        password <- decodedParam "password"
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
        
      "save" -> vaultSaveAction
            
      _ -> redirect "/vault"
  where
    decodedParam p = fromMaybe "" <$> getPostParam p

vaultSaveAction :: AppHandler ()
vaultSaveAction = do
  
  render "vault"
  
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
-- | Renders the echo page.
echo :: Handler App App ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", index)
         , ("/post/:post", showPost)
         , ("/about", aboutMe)
         , ("/vault", method POST vaultAction)
         , ("/vault", vault)
         , ("/vault/edit", vaultEdit)
         , ("/vault/edit/:id", vaultEdit)
         , ("/echo/:stuff", echo)
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
    return App {
        _heist = h,
        _dbLens = _dblens',
        _sessLens = _sesslens'
      }
    where
        commonSplices = bindSplices [
          ("navigation", navigationSplice)] defaultHeistState
          

