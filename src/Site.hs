{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import qualified Site.Common.Splices as CommonSplices
import           Site.Front.Blog
import qualified Site.Front.Splices as FrontSplices


------------------------------------------------------------------------------
-- | Render login form
-- TODO remove
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]


------------------------------------------------------------------------------
-- | Handle login submit
-- TODO remove
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
-- TODO remove
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
-- TODO remove
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"



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

staticDirectoryConfig :: DirectoryConfig (AppHandler)
staticDirectoryConfig = simpleDirectoryConfig
  { mimeTypes = staticMimeMap }

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("", serveDirectoryWith staticDirectoryConfig "static")
         , ("/tag/:tag/page/:page", blog)
         , ("/page/:page", blog)
         , ("/tag/:tag", blog)
         , ("/", blog)
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addConfig h HeistConfig
      { hcInterpretedSplices = []
      , hcLoadTimeSplices = CommonSplices.loadTimeSplices
      , hcCompiledSplices = FrontSplices.compiledSplices
      , hcAttributeSplices = []
      , hcTemplates = Map.empty
      }
    -- addAuthSplices auth
    return $ App h s a

