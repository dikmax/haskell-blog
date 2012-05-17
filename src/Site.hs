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
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Database.HDBC.MySQL
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Hdbc
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
------------------------------------------------------------------------------
import           Application
import           Database

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App App ()
index = do
  let indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("posts", latestPostsSplice)
        ]
  ifTop $ heistLocal (bindSplices indexSplices) $ render "index"

latestPostsSplice :: Splice AppHandler
latestPostsSplice = do
   posts <- lift getLatestPosts
   return [Element "div" [("class", "posts")] $ map mapPosts posts]
   where
     mapPosts (Post id title text) = Element "div" [("class", "post")] [
         Element "h1" [("class", "post-title")] [TextNode $ T.pack title],
         Element "div" [("class", "post-body")] $
           either (\a -> [TextNode $ T.pack a]) extractData $ parseHTML "post" $ pack text
       ]
     extractData (HtmlDocument _ _ docContent) = docContent
     extractData (XmlDocument _ _ docContent) = docContent

aboutMe :: Handler App App ()
aboutMe = render "about"

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
-- | For your convenience, a splice which shows the start time.
startTimeSplice :: Splice AppHandler
startTimeSplice = do
    time <- lift $ gets _startTime
    return [TextNode $ T.pack $ show time]


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the current time.
currentTimeSplice :: Splice AppHandler
currentTimeSplice = do
    time <- liftIO getCurrentTime
    return [TextNode $ T.pack $ show time]


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
         , ("/about", aboutMe)
         , ("/echo/:stuff", echo)
         , ("", with heist heistServe)
         , ("", serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    sTime <- liftIO getCurrentTime -- TODO Remove sTime
    h <- nestSnaplet "heist" heist $ heistInit' "templates" commonSplices
    let mysqlConnection = connectMySQL $ MySQLConnectInfo "127.0.0.1" "root" "" "hblog" 3306 "" Nothing
    _dblens' <- nestSnaplet "hdbc" dbLens $ hdbcInit mysqlConnection
    addRoutes routes
    return $ App h sTime _dblens'
    where
        commonSplices = bindSplices [
          ("navigation", navigationSplice)] defaultHeistState


