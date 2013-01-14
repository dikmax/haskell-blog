{-# LANGUAGE OverloadedStrings #-}
module Site.Snaplet.CommonData where

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Snaplet
import           Snap.Core
import           Snap

data CommonData = CommonData
  { cdTitle :: Text
  } deriving (Show)

class HasCommonData b where
  commonDataLens :: SnapletLens b CommonData

emptyData :: CommonData
emptyData = CommonData ""

commonDataInit :: SnapletInit b CommonData
commonDataInit = makeSnaplet "commonData" "Common data snaplet" Nothing $ do
  return emptyData

getCommonData :: HasCommonData b => Handler b b CommonData
getCommonData = with commonDataLens Snap.get

setCommonData :: HasCommonData b => CommonData -> Handler b b ()
setCommonData = with commonDataLens . Snap.put

modifyCommonData :: HasCommonData b => (CommonData -> CommonData) -> Handler b b ()
modifyCommonData = with commonDataLens . Snap.modify

getTitle :: HasCommonData b => Handler b b Text
getTitle = do
  cd <- getCommonData
  return $ cdTitle cd

setTitle title = modifyCommonData (\cd -> cd {cdTitle = title})
