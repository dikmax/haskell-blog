{-# LANGUAGE OverloadedStrings #-}

module Site.Vault.Files where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as M
import Data.Maybe
import Foreign.C.Types
import Foreign.Ptr
import Network.Curl
import Text.JSON
import Snap.Core (getPostParam, writeBS)
import Snap.Snaplet.Heist
import Snap.Util.FileUploads
import System.Directory (getTemporaryDirectory)
import System.IO

import Application
import Config

data ServiceResponse
  = ServiceError String
  | JsonResult JSValue
  | ServiceDebug String 
  | ServiceSuccess  
    deriving (Show)
  
instance JSON ServiceResponse where
  showJSON (ServiceError message) = makeObj 
    [ ("success", JSBool False)
    , ("message", JSString $ toJSString message)
    ]
  showJSON (JsonResult json) = makeObj 
    [ ("success", JSBool True)
    , ("result", json)
    ]
  showJSON (ServiceDebug str) = makeObj 
    [ ("success", JSBool True)
    , ("message", JSString $ toJSString str)
    ]
  showJSON ServiceSuccess = makeObj 
    [ ("success", JSBool True)
    ]

  readJSON _ = Error "Can't convert JSON to ServiceResponse"

data ServiceAction
  = GetContainers
  | GetContainerItems String
  | UploadFile FilePath String String

vaultFiles :: AppHandler ()
vaultFiles = render "vaultfiles"

vaultFilesService :: AppHandler ()
vaultFilesService = do
  action <- getPostParam "action"
  
  response <- processAction action
  writeBS $ pack $ encode response
  where
    processAction (Just "getContainersList") = liftIO $ curlDo GetContainers
    processAction (Just "getContainerItems") = do
      container <- getPostParam "container"
      case container of
        Just a -> liftIO $ curlDo $ GetContainerItems $ unpack a
        Nothing -> return $ ServiceError "Container not defined"
    processAction _ = return $ ServiceError "Unknown action"
    
curlDo :: ServiceAction -> IO ServiceResponse
curlDo action = withCurlDo $ do
    h <- initialize
    response <- curl h "https://auth.api.rackspacecloud.com/v1.0"
      [CurlHttpHeaders 
        [ "X-Auth-Key: "  ++ rackspaceAuthKey
        , "X-Auth-User: " ++ rackspaceAuthUser
        ]
      ]
    
    let headers = M.map (dropWhile (== ' ')) $ M.fromList $ respHeaders response
    case respStatus response of
      204 -> processAction action 
        (headers M.! "X-Storage-Url") (headers M.! "X-CDN-Management-Url") 
        (headers M.! "X-Auth-Token")
      _ -> return $ ServiceError "Can't authenticate"  
  where
    processAction GetContainers _ cdn = getContainers cdn
    processAction (GetContainerItems container) url _ = 
      getContainerItems container url
    processAction (UploadFile filePath container name) url _ =
      uploadFile filePath container name url
    -- processAction _ = \_ _ -> return $ ServiceError "Internal error"

    getContainers :: String -> String -> IO ServiceResponse
    getContainers url token = do
      h <- initialize
      response <- curl h (url ++ "/?enabled_only=true&format=json") [CurlHttpHeaders ["X-Auth-Token: " ++ token]]
      case respStatus response of
        200 -> return $ fromResult $ decode $ respBody response
        _ -> return $ ServiceDebug $ show $ respStatus response -- TODO Errors handling
    
    getContainerItems :: String -> String -> String -> IO ServiceResponse
    getContainerItems container url token = do
      h <- initialize
      response <- curl h (url ++ "/" ++ container ++ "?format=json") [CurlHttpHeaders ["X-Auth-Token: " ++ token]]
      case respStatus response of
        200 -> return $ fromResult $ decode $ respBody response
        _ -> return $ ServiceDebug $ show $ respStatus response -- TODO Errors handling

    uploadFile filePath container name url token = do
      h <- initialize
      let 
        processFile fh = do
          let 
            readFunction :: Ptr CChar -> CInt -> CInt -> Ptr () -> IO (Maybe CInt)
            readFunction ptr size nmemb _ = do
              actualSize <- hGetBuf fh ptr $ fromInteger $ toInteger (size * nmemb)
              return $ if (actualSize > 0) then Just $ fromInteger $ toInteger actualSize else Nothing

          fileSize <- hFileSize fh
          curl h (url ++ "/" ++ container ++ "/" ++ name)
            [ CurlPut True
            , CurlHttpHeaders ["X-Auth-Token: " ++ token]
            , CurlReadFunction readFunction
            , CurlInFileSize $ fromInteger fileSize
            ]

      response <- withBinaryFile filePath ReadMode processFile
      case respStatus response of
        201 -> return ServiceSuccess
        _ -> return $ ServiceDebug $ show $ respStatus response -- TODO Errors handling

    fromResult (Ok a) = JsonResult a
    fromResult (Error e) = ServiceError e
    
    curl :: Curl -> URLString -> [CurlOption] -> IO CurlResponse
    curl = do_curl_

vaultFileUpload :: AppHandler ()
vaultFileUpload = do
  tmpDir <- liftIO getTemporaryDirectory
  response <- handleFileUploads tmpDir uploadPolicy partUploadPolicy processForm
  writeBS $ pack $ encode response
  where
    uploadPolicy = setMaximumFormInputSize (100 * 1048576) defaultUploadPolicy

    partUploadPolicy (PartInfo fieldName _ _)
      | fieldName == "file" = allowWithMaximumSize (100 * 1048576)
      | otherwise = disallow

    -- processForm :: [(PartInfo, Either PolicyViolationException FilePath)] -> AppHandler ServiceResponse
    processForm ((_, Left uploadError) : []) = 
      return $ ServiceError $ show uploadError
    processForm ((_, Right path) : []) = do
      container <- getPostParam "container"
      name <- getPostParam "name"
      if container == Nothing || name == Nothing
        then return $ ServiceError "Container or name not defined"
        else liftIO $ curlDo $ UploadFile path (unpack $ fromMaybe "" container) (unpack $ fromMaybe "" name)

    processForm [] = return $ ServiceError "No files were transmitted"
    processForm _ = return $ ServiceError "Too many files"
