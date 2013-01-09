{-# LANGUAGE OverloadedStrings #-}

module Site.Front.Blog where

import           Snap.Snaplet.Heist

import           Application

blog :: AppHandler ()
blog = cRender "blog"

{-
index :: Handler App App ()
index =  ifTop $ do
  page <- decodedParam "page"
  tag <- getParam "tag"
  let
    tagText = T.decodeUtf8' $ fromMaybe "" tag
    tagLink
      | isNothing tag = Nothing
      | otherwise = case tagText of
        (Left _) -> Nothing
        (Right t) -> Just t
    pageNum = case reads $ unpack page of
      [(x, "")] -> x
      _ -> 1
    indexSplices =
      [ ("posts", postsSplice pageNum tag)
      , ("pagination", paginationSplice pageNum tag)
      , ("metadata", metadataSplice $ defaultMetadata
        { metaUrl = maybe "" (\t -> "/tag/" `T.append` t) tagLink
            `T.append` if pageNum == 1 then "" else "/page/" `T.append` T.pack (show pageNum)
        , metaDescription = makeDescription tagLink pageNum
        , metaTitle = makeTitle tagLink pageNum
        , metaType = FacebookBlog
        })
      , ("rss", rssSplice tag)
      ]

    makeDescription Nothing 1 = "Мой персональный блог. Я рассказываю о программировании и иногда о своей жизни."
    makeDescription Nothing p = "Мой персональный блог, записи с " `T.append`
      T.pack (show ((p - 1) * postsPerPage + 1)) `T.append` " по " `T.append`
      T.pack (show (p * postsPerPage)) `T.append` "."
    makeDescription (Just t) 1 = "Мой персональный блог, записи с тегом \"" `T.append` t `T.append` ".\""
    makeDescription (Just t) p = "Мой персональный блог, записи с тегом \"" `T.append` t `T.append` "\" c " `T.append`
      T.pack (show ((p - 1) * postsPerPage + 1)) `T.append` " по " `T.append`
      T.pack (show (p * postsPerPage))`T.append` "."

    makeTitle Nothing 1 = Nothing
    makeTitle Nothing p = Just $ T.pack (show p) `T.append` "-я страница"
    makeTitle (Just t) 1 = Just $ "\"" `T.append` t `T.append` "\""
    makeTitle (Just t) p = Just $ "\"" `T.append` t `T.append` "\", " `T.append` T.pack (show p) `T.append`
      "-я страница"

  either
    (\_ -> error404)
    (\_ -> heistLocal (I.bindSplices indexSplices) $ render "index")
    tagText
-}