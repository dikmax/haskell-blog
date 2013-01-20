------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans (lift)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.HDBC.MySQL
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import           Site.Common.Config
import qualified Site.Common.Splices as CommonSplices
import           Site.Database
import           Site.Front.Blog
import qualified Site.Front.Splices as FrontSplices
import           Site.Snaplet.CommonData
import           Site.Snaplet.I18N
import           Site.Types
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


prepareCommonData :: AppHandler ()
prepareCommonData = do
  serverName <- withRequest (return . rqServerName)
  blog <- getBlog $ T.decodeUtf8 serverName
  if blog /= UnknownBlog then setBlog blog else redirect defaultDomain



------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    cd <- nestSnaplet "" commonData $ commonDataInit

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    db <- nestSnaplet "" hdbc $ hdbcInit $ connectMySQL connectInfo
    i <- nestSnaplet "" i18n $ i18nInit

    addRoutes routes
    addConfig h HeistConfig
      { hcInterpretedSplices = []
      , hcLoadTimeSplices = CommonSplices.loadTimeSplices
      , hcCompiledSplices = CommonSplices.compiledSplices
      , hcAttributeSplices = []
      , hcTemplates = Map.empty
      }

    wrapSite (setLanguage "en" *>)
    wrapSite (setEncoding *>)
    wrapSite (prepareCommonData *>)

    -- addAuthSplices auth
    return $ App
      { _heist = h
      , _sess = s
      , _commonData = cd
      , _hdbc = db
      , _i18n = i
      , _auth = a
      }
