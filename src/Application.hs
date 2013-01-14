{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Database.HDBC.MySQL
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Hdbc
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
------------------------------------------------------------------------------
import Site.Snaplet.CommonData

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _commonData :: Snaplet CommonData
    , _hdbc :: Snaplet (HdbcSnaplet Connection IO)
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasCommonData App where
  commonDataLens = commonData

------------------------------------------------------------------------------
type AppHandler = Handler App App


