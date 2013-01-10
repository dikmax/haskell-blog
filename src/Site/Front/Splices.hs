{-# LANGUAGE OverloadedStrings #-}
module Site.Front.Splices where

import           Control.Monad.Trans
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Heist.Compiled
import           Text.Blaze.Renderer.Utf8
import qualified Text.Blaze.XHtml5 as H
import           Text.Blaze.XHtml5
import qualified Text.Blaze.XHtml5.Attributes as A
import           Text.Blaze.XHtml5.Attributes

import           Application


-- | Splice to show years in copyright
copyrightYearSplice :: Integer -> Splice AppHandler
copyrightYearSplice startYear = do
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
    getYear time = getYear_ $ toGregorian $ localDay $ utcToLocalTime (minutesToTimeZone 180) time
    getYear_ (year, _, _) = year

compiledSplices :: [(Text, Splice AppHandler)]
compiledSplices =
  [ ("copyrightYear", copyrightYearSplice 2012)
  ]