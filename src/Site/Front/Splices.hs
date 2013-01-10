{-# LANGUAGE OverloadedStrings #-}
module Site.Front.Splices where

import           Control.Monad.Trans
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Heist.Compiled
import           Text.XmlHtml

import           Application
import qualified HtmlTags as H
import           HtmlTags ((<@), (<.), (<#), (<&), (<&&))
import qualified HtmlAttributes as A


-- | Splice to show years in copyright
copyrightYearSplice :: Integer -> Splice AppHandler
copyrightYearSplice startYear = do
    return $ yieldRuntime $ do
        currentTime <- liftIO getCurrentTime
        return $ renderHtmlFragment UTF8
           [
              H.span <@ A.itemprop "copyrightYear" <#
                (
                  if (startYear == getYear currentTime)
                    then
                      T.pack (show startYear)
                    else
                      T.pack (show startYear) `T.append`
                      " — " `T.append`
                      T.pack (show $ getYear currentTime)
                )
           ]
    where
      getYear time = getYear_ $ toGregorian $ localDay $ utcToLocalTime (minutesToTimeZone 180) time
      getYear_ (year, _, _) = year

--copyrightYearSplice startYear = do
--  currentTime <- liftIO getCurrentTime
--  return
--    [
--      H.span <@ A.itemprop "copyrightYear" <#
--        (
--          if (startYear == getYear currentTime)
--            then
--              T.pack (show startYear)
--            else
--              T.pack (show startYear) `T.append`
--              " — " `T.append`
--              T.pack (show $ getYear currentTime)
--        )
--    ]
--  where
--    getYear time = getYear_ $ toGregorian $ localDay $ utcToLocalTime (minutesToTimeZone 180) time
--    getYear_ (year, _, _) = year

-- [(T.Text, SnapletISplice App)]
compiledSplices :: [(Text, Splice AppHandler)]
compiledSplices =
    [ ("copyrightYear", copyrightYearSplice 2012)
    ]