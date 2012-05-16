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
import           Data.ByteString.Char8 (unpack)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
------------------------------------------------------------------------------
import           Application

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App App ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices =
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]

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
    sTime <- liftIO getCurrentTime
    h <- nestSnaplet "heist" heist $ heistInit' "templates" commonSplices
    addRoutes routes
    return $ App h sTime
    where
        commonSplices = bindSplices [
          ("navigation", navigationSplice)] defaultHeistState


