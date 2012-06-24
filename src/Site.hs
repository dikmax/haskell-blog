{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder (toByteString)
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
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           System.Locale
import           Text.Pandoc
import           Text.Pandoc.Highlighting
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
------------------------------------------------------------------------------
import           Application
import           Config
import           Database
import           Site.Rss
import           Site.Vault.Files
import           Site.Utils
import           Types

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
    prevLink = maybe "/" (\t -> "/tag/" ++ unpack t) tag ++ 
      "page/" ++ show (page + 1)
    nextDisabled = page <= 1
    nextLink = maybe "/" (\t -> "/tag/" ++ unpack t) tag ++ 
      if page == 2 then "" else "page/" ++ show (page - 1)
    prevElement = if prevDisabled
      then []
      else [Element "li" [("class", "previous")]
        [ Element "a" [("href", T.pack prevLink)] [TextNode "← Старше"] ] ]
    nextElement = if nextDisabled      
      then []
      else [Element "li" [("class", "next")]
        [ Element "a" [("href", T.pack nextLink)] [TextNode "Моложе →"] ] ]
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

renderTags :: [Text] -> [Node]
renderTags [] = []
renderTags tags = [Element "div" [("class", "post-tags")] $ renderTags' tags]
  where
    renderTags' (t:[]) = [Element "a" [("href", "/tag/" `T.append` t)]
      [TextNode t]]
    renderTags' (t:ts) = [Element "a" [("href", "/tag/" `T.append` t)]
      [TextNode t], TextNode ", "] ++ renderTags' ts
    renderTags' _ = []

parserState :: ParserState
parserState = defaultParserState 
  { stateSmart = True
  , stateParseRaw = True
  }
  
writerOptions :: WriterOptions
writerOptions = defaultWriterOptions
  { writerHighlight = True,
    writerHighlightStyle = kate
  }

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
  return $ map renderPost posts
  where 
    renderPost post = Element "tr" 
      [ ("data-rowid", T.pack $ show $ postId post)
      , ("data-url", postUrl post) ] 
      [ Element "td" [] [TextNode $ T.pack $ show $ postDate post]
      , Element "td" [] [TextNode $ if postPublished post then "+" else ""]
      , Element "td" [] 
        [ TextNode $ postTitle post
        , Element "div" [] $ renderTags $ postTags post
        ]
      , Element "td" [("class", "actions")] 
        [ Element "span" [("class", "action-view")] []
        , Element "span" [("class", "action-delete")] []
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
    getPost' "" = liftIO newPost
    getPost' id = getPostById id
    
-- TODO there should be a way to simplify this function
vaultGetPost :: AppHandler Post
vaultGetPost = do
  id <- decodedPostParam "id"
  title <- decodedPostParam "title"
  text <- decodedPostParam "text"
  url <- decodedPostParam "url"
  date <- decodedPostParam "date"
  published <- decodedPostParam "published"
  special <- decodedPostParam "special"
  tags <- decodedPostParam "tags"  
  return Post 
      { postId = read $ unpack id -- TODO validate
      , postTitle = T.decodeUtf8 title
      , postText = T.replace "\r\n" "\n" $ T.decodeUtf8 text
      , postDate = read $ unpack date -- TODO check for format errors
      , postUrl = T.decodeUtf8 url -- TODO check for duplicate
      , postPublished = published /= ""
      , postSpecial = special /= ""
      , postTags = stringToTags $ T.decodeUtf8 tags
      }


vaultSave :: AppHandler ()
vaultSave = do
  post <- vaultGetPost
  savePost post
  redirect "/vault"

-- TODO digestive functors
vaultPostForm :: Post -> Splice AppHandler
vaultPostForm (Post id title text url date published special tags) =
  return 
    [ Element "form" 
      [ ("class", "form-horizontal post-form")
      , ("method", "post")
      ] 
      [ Element "input" 
        [ ("type", "hidden")
        , ("name", "id")
        , ("value", T.pack $ show id)
        ] []
      , Element "fieldset" [] 
        [ Element "legend" [] [TextNode "Редактирование записи"]
        , inputText "Заголовок" "title" title
        , inputText "Url" "url" url
        , inputText "Дата" "date" $ T.pack $ show date
        , inputCheckbox "Опубликовано" "published" published
        , inputCheckbox "Специальный" "special" special
        , textarea "Текст" "text" text
        , inputText "Теги" "tags" $ tagsToString tags
        , Element "div" [("class", "form-actions")] 
          [ Element "button" 
            [ ("type", "submit")
            , ("class", "btn btn-primary")
            ] 
            [TextNode "Сохранить"]
          , Element "button" [("class", "btn")] [TextNode "Отмена"]
          , Element "button" 
            [ ("class", "btn btn-refresh") ] 
            [ TextNode "Обновить" ]
          ]
        ]
      ]
    ]
  where
    inputText :: Text -> Text -> Text -> Node
    inputText fieldLabel name value = field fieldLabel name [
        Element "input" [("type", "text"), ("name", name), 
          ("class", "input-fullwidth"), ("id", "post-" `T.append` name), 
          ("value", value)] []
      ]
    inputCheckbox :: Text -> Text -> Bool -> Node
    inputCheckbox fieldLabel name value = field fieldLabel name [
        Element "input" 
          ([("type", "checkbox"), ("name", name), 
            ("id", "post-" `T.append` name)] ++ 
              [("checked", "checked") | value])  
          []
      ]
    textarea :: Text -> Text -> Text -> Node
    textarea fieldLabel name value = field fieldLabel name [
        Element "textarea" [("name", name),  
          ("id", "post-" `T.append` name),
          ("class", "input-fullwidth monospace"), ("rows", "20")] 
          [TextNode value]
      ]
    field :: Text -> Text -> [Node] -> Node
    field fieldLabel fieldName fieldControl =
      Element "div" [("class", "control-group")] [
        Element "label" [("class", "control-label"), 
          ("for", "post-" `T.append` fieldName)] [
          TextNode fieldLabel
        ],
        Element "div" [("class", "controls")] fieldControl
      ]

vaultRenderPost :: AppHandler ()
vaultRenderPost = do
  post <- vaultGetPost 
  writeBS $ toByteString $ renderHtmlFragment UTF8 [renderSinglePost post]
  
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
        else heistLocal (bindString "error" "Неверные данные") $ 
          render "vaultlogin"

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
siteStructure = [("/", "Главная"), ("/about", "Обо мне")]

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
  
