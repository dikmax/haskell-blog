{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import Control.Monad.State
import Data.Lens.Template
import Database.HDBC.MySQL
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Hdbc
import Snap.Snaplet.Session 

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sessLens :: Snaplet SessionManager    
    , _dbLens :: Snaplet (HdbcSnaplet Connection IO)       
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasHdbc (Handler b App) Connection IO where
  getHdbcState = with dbLens get
      
------------------------------------------------------------------------------
type AppHandler = Handler App App


