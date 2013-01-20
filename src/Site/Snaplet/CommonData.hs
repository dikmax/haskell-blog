module Site.Snaplet.CommonData where

------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap.Snaplet
import           Snap.Core
import           Snap
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
  return emptyData

getCommonData :: HasCommonData b => Handler b b CommonData
getCommonData = with commonDataLens Snap.get

setCommonData :: HasCommonData b => CommonData -> Handler b b ()
setCommonData = with commonDataLens . Snap.put

modifyCommonData :: HasCommonData b => (CommonData -> CommonData) -> Handler b b ()
modifyCommonData = with commonDataLens . Snap.modify

-- getData :: HasCommonData b => (CommonData -> a) -> Handler b b Text
getData f = getCommonData >>= return . f

setTitle :: HasCommonData b => Text -> Handler b b ()
setTitle title = modifyCommonData (\cd -> cd {cdTitle = title})

setBlog :: HasCommonData b => Blog -> Handler b b ()
setBlog blog = modifyCommonData (\cd -> cd {cdBlog = blog})