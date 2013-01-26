{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Monad.State (get)
import Control.Lens
import Data.Pool
import Database.HDBC.MySQL
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Hdbc
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
------------------------------------------------------------------------------
import Site.Snaplet.CommonData
import Site.Snaplet.DbCache
import Site.Snaplet.I18N

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _commonData :: Snaplet CommonData
    , _hdbc :: Snaplet (HdbcSnaplet Connection Pool)
    , _i18n :: Snaplet I18N
    , _auth :: Snaplet (AuthManager App)
    , _dbCache :: Snaplet DbCacheRef
    }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasCommonData App where
  commonDataLens = commonData

instance HasDbCache App where
  dbCacheLens = dbCache

instance HasI18N App where
  i18nLens = i18n

instance HasHdbc (Handler b App) Connection Pool where
  getHdbcState = with hdbc get

------------------------------------------------------------------------------
type AppHandler = Handler App App


