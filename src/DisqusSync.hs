{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intersperse, nub)
import Data.Map ((!), Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format
import Database.HDBC
import Database.HDBC.MySQL
import Network.Curl
import System.Locale
import Text.JSON

import Config



data DisqusComment = DisqusComment
  { dcIsJuliaFlagged :: Bool
  , dcIsFlagged :: Bool
  , dcParent :: Maybe Int
  , dcAuthorName :: Text
  , dcAuthorUrl :: Maybe Text
  , dcAuthorAvatar :: Maybe Text
  -- , dcAuthor :: DisqusAuthor
  -- , dcMedia :: [DisqusMedia]
  , dcIsDeleted :: Bool
  , dcIsApproved :: Bool
  , dcDislikes :: Int
  , dcRawMessage :: Text
  , dcCreatedAt :: Maybe LocalTime
  , dcId :: Text -- Int ????
  , dcThread :: Text
  , dcNumReports :: Int
  , dcIsEdited :: Bool
  , dcLikes :: Int
  , dcPoints :: Int
  , dcMessage :: Text
  , dcIsSpam :: Bool
  , dcIsHighlighted :: Bool
  , dcUserScore :: Int
  , dcMyThread :: Maybe Int
  } deriving (Show)

instance JSON DisqusComment where
  showJSON _ = JSNull

  readJSON (JSObject a@obj)
    | isError getIsJuliaFlagged = toError "isJuliaFlagged not found" getIsJuliaFlagged
    | isError getIsFlagged = toError "isFlagged not found" getIsFlagged
    | isError getParent = toError "parent not found" getParent
    | isError getIsDeleted = toError "isDeleted not found" getIsDeleted
    | isError getIsApproved = toError "isApproved not found" getIsApproved
    | isError getDislikes = toError "dislikes not found" getDislikes
    | isError getRawMessage = toError "raw_message not found" getRawMessage
    | isError getCreatedAt = toError "createdAt not found" getCreatedAt
    | isError getId = toError "id not found" getId
    | isError getThread = toError "thread not found" getThread
    | isError getNumReports = toError "numReports not found" getNumReports
    | isError getIsEdited = toError "isEdited not found" getIsEdited
    | isError getLikes = toError "likes not found" getLikes
    | isError getPoints = toError "points not found" getPoints
    | isError getMessage = toError "message not found" getMessage
    | isError getIsSpam = toError "isSpam not found" getIsSpam
    | isError getIsHighlighted = toError "isHighlighted not found" getIsHighlighted
    | isError getUserScore = toError "userScore not found" getUserScore
    | isError getAuthor = toError "author not found" getAuthor
    | otherwise = Ok DisqusComment
      { dcIsJuliaFlagged = fromResult getIsJuliaFlagged
      , dcIsFlagged = fromResult getIsFlagged
      , dcParent =
        case fromResult getParent of
          JSRational _ b -> Just $ round $ fromRational b
          _ -> Nothing
      , dcAuthorName =
        case valFromObj "name" $ fromResult getAuthor of
          (Ok (JSString str)) -> T.pack $ fromJSString str
          _ -> ""
      , dcAuthorUrl =
        case valFromObj "url" $ fromResult getAuthor of
          (Ok (JSString str)) -> Just $ T.pack $ fromJSString str
          _ -> Nothing
      , dcAuthorAvatar =
        case valFromObj "avatar" $ fromResult getAuthor of
          (Ok (JSObject avatar)) ->
            case valFromObj "permalink" avatar of
              (Ok (JSString str)) -> Just $ T.pack $ fromJSString str
              _ -> Nothing
          _ -> Nothing
      -- , dcAuthor :: DisqusAuthor
      -- , dcMedia :: [DisqusMedia]
      , dcIsDeleted = fromResult getIsDeleted
      , dcIsApproved = fromResult getIsApproved
      , dcDislikes = fromResult getDislikes
      , dcRawMessage = T.pack $ fromResult getRawMessage
      , dcCreatedAt = parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" $ fromResult getCreatedAt
      , dcId = T.pack $ fromResult getId
      , dcThread = T.pack $ fromResult getThread
      , dcNumReports = fromResult getNumReports
      , dcIsEdited = fromResult getIsEdited
      , dcLikes = fromResult getLikes
      , dcPoints = fromResult getPoints
      , dcMessage = T.pack $ fromResult getMessage
      , dcIsSpam = fromResult getIsSpam
      , dcIsHighlighted = fromResult getIsHighlighted
      , dcUserScore = fromResult getUserScore
      , dcMyThread = Nothing
      }
    where
      getIsJuliaFlagged = valFromObj "isJuliaFlagged" obj
      getIsFlagged = valFromObj "isFlagged" obj
      getParent = valFromObj "parent" obj
      getIsDeleted = valFromObj "isDeleted" obj
      getIsApproved = valFromObj "isApproved" obj
      getDislikes = valFromObj "dislikes" obj
      getRawMessage = valFromObj "raw_message" obj
      getCreatedAt = valFromObj "createdAt" obj
      getId = valFromObj "id" obj
      getThread = valFromObj "thread" obj
      getNumReports = valFromObj "numReports" obj
      getIsEdited = valFromObj "isEdited" obj
      getLikes = valFromObj "likes" obj
      getPoints = valFromObj "points" obj
      getMessage = valFromObj "message" obj
      getIsSpam = valFromObj "isSpam" obj
      getIsHighlighted = valFromObj "isHighlighted" obj
      getUserScore = valFromObj "userScore" obj
      getAuthor = valFromObj "author" obj
  readJSON _ = Error "Error in discus comment"

isError (Ok _) = False
isError (Error _) = True

fromResult (Ok a) = a
fromResult (Error str) = error $ "Decode error: " ++ str

toError :: String -> Result a -> Result b
toError prefix (Error str) = Error $ prefix ++ ": " ++ str
toError prefix _ = Error $ prefix ++ ": Access error"

disqusApiKey :: String
disqusApiKey = "eEu5UUONIskKunn9HIudZ8DUpAdPPkbgwsLBzyeVRD4ACEqtOqY1OPdC2cfL7CJ2"

curlDo :: String -> IO String
curlDo url = withCurlDo $ do
  h <- initialize
  response <- curl h url []
  return $ respBody response
  where
    curl :: Curl -> URLString -> [CurlOption] -> IO CurlResponse
    curl = do_curl_


listPosts :: LocalTime -> IO ([DisqusComment], [String]) -- Comments, error messages
listPosts since = do
  postsString <- curlDo url
  let res = decode postsString
  case filterError res of
    (Ok (JSArray arr)) -> return $ transformArray arr
    (Ok other) -> return ([], ["Server should return array but return " ++ encode other])
    (Error str) -> return ([], [str])
  where
    url = "https://disqus.com/api/3.0/forums/listPosts.json?forum=dikmax&api_key=" ++ disqusApiKey ++
      "&order=asc&since=" ++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" since
    transformArray :: [JSValue] -> ([DisqusComment], [String])
    transformArray [] = ([], [])
    transformArray (x:xs) =
      case readJSON x of
        (Ok item) -> (item : fst (transformArray xs), snd $ transformArray xs)
        (Error str) -> (fst $ transformArray xs, str : snd (transformArray xs))

listThreads :: [DisqusComment] -> IO (Map String String, [String])
listThreads comments = do
  threadsString <- curlDo url
  let res = decode threadsString
  case filterError res of
    (Ok (JSArray arr)) -> return $ transformArray arr
    (Ok other) -> return (M.empty, ["Server should return array but return " ++ encode other])
    (Error str) -> return (M.empty, [str])
  where
    url = "https://disqus.com/api/3.0/threads/list.json?forum=dikmax&api_key=" ++ disqusApiKey ++
      concatMap ("&thread=" ++) (newThreads comments)

    transformArray :: [JSValue] -> (Map String String, [String])
    transformArray = transformArray_ M.empty

    transformArray_ :: Map String String -> [JSValue] -> (Map String String, [String])
    transformArray_ m [] = (m, [])
    transformArray_ m (JSObject x : xs)
      | isError $ getId x = (fst $ transformArray_ m xs, "id not found" : snd (transformArray_ m xs))
      | isError $ getIdentifiers x = (fst $ transformArray_ m xs, "identifiers not found" : snd (transformArray_ m xs))
      | otherwise = case getIdentifiers x of
        (Ok (JSArray [])) -> (fst $ transformArray_ m xs, "identifiers is empty" : snd (transformArray_ m xs))
        (Ok (JSArray (JSString str : _))) -> (M.insert (fromJSString $ fromResult $ getId x) (fromJSString str) $ fst $ transformArray_ m xs, snd (transformArray_ m xs))
        _ -> (fst $ transformArray_ m xs, "identifiers is not array" : snd (transformArray_ m xs))
    transformArray_ m (_ : xs) = (fst $ transformArray_ m xs, "Response item is not an object" : snd (transformArray_ m xs))

    getId = valFromObj "id"
    getIdentifiers = valFromObj "identifiers"

newThreads comments = map T.unpack $ nub $ map dcThread [ x | x <- comments, isNothing $ dcMyThread x ]

filterError :: Result JSValue -> Result JSValue
filterError res@(Ok (JSObject obj))
  | getCode obj == Just 0 = valFromObj "response" obj
  | fromMaybe 0 (getCode obj) > 0 =
    case (valFromObj "response" obj :: Result JSValue) of
      (Ok (JSString str)) -> Error $ fromJSString str
      _ -> Error "Error with no message"
  | otherwise = Error $ show res
  where
    getCode obj = getCode_ $ valFromObj "code" obj
    getCode_ (Ok (JSRational False a)) = Just a
    getCode_ _ = Nothing
filterError (Ok a) = Error $ "Server should return object but returned: " ++ show a
filterError e = e

minDate = LocalTime (fromGregorian 2012 3 1) midnight
-- Database functions
getSince :: (IConnection a) => a -> IO LocalTime
getSince conn = do
  stmt <- prepare conn "SELECT MAX(date) FROM comments"
  executeRaw stmt
  res <- fetchRow stmt
  finish stmt
  case res of
    Just [SqlNull] -> return minDate
    Just [v] -> return $ fromSql v
    _ -> return minDate

getKnownThreads :: (IConnection a) => a -> [DisqusComment] -> IO [DisqusComment]
getKnownThreads _ [] = return []
getKnownThreads conn comments
  | newComments == [] = return comments
  | otherwise = do
    stmt <- prepare conn ("SELECT id, disqus_thread " ++
      "FROM posts " ++
      "WHERE disqus_thread IN ("  ++
      intersperse ',' (map (\_ -> '?') newComments) ++ ")")
    execute stmt $ map toSql newComments
    updateData stmt comments
  where
    newComments = newThreads comments

    updateData stmt comments = do
      row <- fetchRowMap stmt
      case row of
        Just rw -> updateData stmt $
          map (updateComment (fromSql $ rw ! "id") (fromSql $ rw ! "disqus_thread")) comments
        Nothing -> return comments

    updateComment pId disqusThread comment
      | dcThread comment == disqusThread = comment
        { dcMyThread = Just pId }
      | otherwise = comment

saveThreads :: (IConnection a) => a -> Map String String -> IO ()
saveThreads conn m
  | M.null m = return ()
  | otherwise = withTransaction conn $ saveThreads_ $ M.assocs m
  where
    saveThreads_ [] _ = return ()
    saveThreads_ ((k, v) : ms) conn = do
      stmt <- prepare conn "UPDATE posts SET disqus_thread = ? WHERE url = ?"
      execute stmt [toSql k, toSql v]
      saveThreads_ ms conn

saveComments :: (IConnection a) => a -> [DisqusComment] -> IO ()
saveComments conn comments = withTransaction conn $ saveComments_ comments
  where
    saveComments_ [] _ = return ()
    saveComments_ (c:cs) conn
      | isJust $ dcMyThread c = do
        stmt <- prepare conn ("REPLACE comments (comment_id, thread_id, " ++
          "parent_comment_id, body, author_name, " ++
          "author_avatar, author_url, date) VALUES (?, ?, ?, ?, ?, ?, ?, ?)")
        execute stmt [toSql $ dcId c, toSql $ fromMaybe 0 $ dcMyThread c,
          maybe SqlNull toSql $ dcParent c,
          toSql $ dcMessage c, toSql $ dcAuthorName c,
          toSql $ fromMaybe "" $ dcAuthorAvatar c,
          toSql $ fromMaybe "" $ dcAuthorUrl c,
          toSql $ fromMaybe minDate $ dcCreatedAt c
          ]
        saveComments_ cs conn
      | otherwise = saveComments_ cs conn

main :: IO ()
main = do
  conn <- connectMySQL connectInfo
  runRaw conn "SET NAMES utf8"
  -- Get last comment date in our db
  since <- getSince conn
  -- Get new comments from disqus
  (posts, postErrors) <- listPosts since
  writeErrors "listPosts errors:" postErrors
  -- Complement comments from disqus with our post ids
  updatedPosts <- getKnownThreads conn posts
  -- Read new urls of posts with comments
  (threads, threadErrors) <- listThreads updatedPosts
  writeErrors "listThreads errors:" threadErrors
  -- Write links between disqus threads and our threads
  saveThreads conn threads
  -- Complement comments with just written post ids
  updatedPosts2 <- getKnownThreads conn updatedPosts
  -- Save
  saveComments conn updatedPosts2

  -- Done
  disconnect conn
  where
    writeErrors _ [] = return ()
    writeErrors t e = do
      putStrLn t
      writeErrors_ e

    writeErrors_ [] = return ()
    writeErrors_ (e:es) = do
      putStrLn e
      writeErrors_ es
