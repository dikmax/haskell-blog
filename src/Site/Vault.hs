{-# LANGUAGE OverloadedStrings #-}

module Site.Vault where

import Prelude hiding (id)

import Blaze.ByteString.Builder (toByteString)
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Hdbc
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Text.Templating.Heist
import Text.XmlHtml hiding (render)

import Application
import Config
import Database
import Site.Common
import Site.Utils
import Types

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
        , ("id", "post-id")
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

vaultCheckUrl :: AppHandler ()
vaultCheckUrl = do
  id' <- decodedPostParam "id"
  url <- decodedPostParam "url"
  result <- vaultValidateUrl (T.decodeUtf8 id') (T.decodeUtf8 url)
  case result of
    True -> writeBS "{\"result\":true}"
    False -> writeBS "{\"result\":false}"
  
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
