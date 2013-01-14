{-# LANGUAGE OverloadedStrings #-}
module Site.Common.Splices where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Heist.Compiled
import qualified Heist.Interpreted as I
import           Snap.Core
import           Text.Blaze.Renderer.Utf8
import qualified Text.Blaze.XHtml5 as H
import           Text.Blaze.XHtml5
import qualified Text.Blaze.XHtml5.Attributes as A
import           Text.Blaze.XHtml5.Attributes
import           Text.XmlHtml
------------------------------------------------------------------------------
import           Application
import           Site.Common.Config
import           Site.Snaplet.CommonData


------------------------------------------------------------------------------

-- | Splice to show years in copyright
copyrightYearSplice :: Integer -> Splice AppHandler
copyrightYearSplice startYear =
  return $ yieldRuntime $ do
    currentTime <- liftIO getCurrentTime
    return $ renderMarkupBuilder $
      H.span ! itemprop "copyrightYear" $
        if (startYear == getYear currentTime)
          then
            toMarkup startYear
          else do
            toMarkup startYear
            " — "
            toMarkup $ getYear currentTime
  where
    getYear time = getYear_ $ toGregorian $ localDay $
        utcToLocalTime (minutesToTimeZone 180) time
    getYear_ (year, _, _) = year


metadataSplice :: Splice AppHandler
metadataSplice =
  return $ yieldRuntime $ do
    title <- lift $ getTitle
    return $ renderMarkupBuilder $
      H.title $ toMarkup title

-- | Splice to detect is userAgent is mobile
mobileSplice :: Splice AppHandler
mobileSplice =
  return $ yieldRuntimeText $ do
    userAgent <- lift $ withRequest (return . T.decodeUtf8 . fromMaybe "" .
            getHeader "User-Agent")
    return $
      if "Opera Mobi" `T.isInfixOf` userAgent ||
         "Opera Mini" `T.isInfixOf` userAgent ||
         "Android" `T.isInfixOf` userAgent ||
         "Silk" `T.isInfixOf` userAgent ||
         "PlayBook" `T.isInfixOf` userAgent ||
         "iPad" `T.isInfixOf` userAgent ||
         "iPhone" `T.isInfixOf` userAgent ||
         "iPod" `T.isInfixOf` userAgent ||
         (not ("Opera" `T.isPrefixOf` userAgent) &&
             ("WebKit" `T.isInfixOf` userAgent) &&
             ("Mobile" `T.isInfixOf` userAgent))
      then "mobile" else "no-mobile"


-- | Splice to show resources revision. Revision is actually server
-- start time timestamp.
revisionSplice :: I.Splice IO
revisionSplice = do
  posixTime <- lift getPOSIXTime
  return [TextNode $ T.pack $ show $ round posixTime]

------------------------------------------------------------------------------

compiledSplices :: [(Text, Splice AppHandler)]
compiledSplices =
  [ ("copyrightYear", copyrightYearSplice 2012)
  , ("mobile", mobileSplice)
  , ("metadata", metadataSplice)
  ]

loadTimeSplices :: [(Text, I.Splice IO)]
loadTimeSplices =
  [ ("revision", revisionSplice)
  ]