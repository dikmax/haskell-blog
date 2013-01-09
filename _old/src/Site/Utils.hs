{-# LANGUAGE OverloadedStrings #-}

module Site.Utils where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Maybe
import Snap.Core

decodedParam :: MonadSnap f => ByteString -> f ByteString
decodedParam p = fromMaybe "" <$> getParam p

decodedPostParam :: MonadSnap f => ByteString -> f ByteString
decodedPostParam p = fromMaybe "" <$> getPostParam p
