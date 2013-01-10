{-# LANGUAGE OverloadedStrings #-}

module Site.Front.Splices where

import Heist.Compiled
import Data.Text (Text, pack)

import Application

-- | Splice to show years in copyright
copyrightYearSplice :: Integer -> Splice AppHandler
copyrightYearSplice startYear = do
    return $ yieldRuntimeText $ do
        -- currentTime <- liftIO getCurrentTime
        return $ pack $ show $ startYear

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