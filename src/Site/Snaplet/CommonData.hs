{-# LANGUAGE RankNTypes #-}
module Site.Snaplet.CommonData where

------------------------------------------------------------------------------
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Snap.Snaplet
import           Snap
import           Data.Aeson.Generic
------------------------------------------------------------------------------
import           Site.Types
------------------------------------------------------------------------------

data CommonData = CommonData
  { cdTitle :: Text
  , cdBlog :: Blog
  } deriving (Show)

class HasCommonData b where
  commonDataLens :: SnapletLens b CommonData

emptyData :: CommonData
emptyData = CommonData
  { cdTitle = ""
  , cdBlog = UnknownBlog
  }

commonDataInit :: SnapletInit b CommonData
commonDataInit = makeSnaplet "commonData" "Common data snaplet" Nothing $ do
  printInfo $ T.decodeUtf8 $ toStrict $ encode $ PageOptions True "a" "б"
  return emptyData

getCommonData :: HasCommonData b => Handler b b CommonData
getCommonData = with commonDataLens Snap.get

setCommonData :: HasCommonData b => CommonData -> Handler b b ()
setCommonData = with commonDataLens . Snap.put

modifyCommonData :: HasCommonData b => (CommonData -> CommonData) -> Handler b b ()
modifyCommonData = with commonDataLens . Snap.modify

-- getData :: HasCommonData b => (CommonData -> a) -> Handler b b Text
getData :: forall b b1. HasCommonData b1 => (CommonData -> b) -> Handler b1 b1 b
getData f = getCommonData >>= return . f

setTitle :: HasCommonData b => Text -> Handler b b ()
setTitle title = modifyCommonData (\cd -> cd {cdTitle = title})

setBlog :: HasCommonData b => Blog -> Handler b b ()
setBlog blog = modifyCommonData (\cd -> cd {cdBlog = blog})


toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks
