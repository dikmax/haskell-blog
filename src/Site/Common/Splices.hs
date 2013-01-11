{-# LANGUAGE OverloadedStrings #-}
module Site.Common.Splices where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX
import           Heist.Compiled as C
import qualified Heist.Interpreted as I
import           Text.Blaze.Renderer.Utf8
import qualified Text.Blaze.XHtml5 as H
import           Text.Blaze.XHtml5
import qualified Text.Blaze.XHtml5.Attributes as A
import           Text.Blaze.XHtml5.Attributes
import           Text.XmlHtml
------------------------------------------------------------------------------
import           Application
import           Site.Common.Config


------------------------------------------------------------------------------

revisionSplice :: I.Splice IO
revisionSplice = do
  posixTime <- lift getPOSIXTime
  return [TextNode $ T.pack $ show $ round posixTime]

loadTimeSplices :: [(Text, I.Splice IO)]
loadTimeSplices =
  [ ("revision", revisionSplice)
  ]