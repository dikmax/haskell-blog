--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_, filterM)
import           Data.List (sortBy, intercalate, unfoldr)
import qualified Data.Map as M
import           Data.Monoid (mappend)
-- import           Data.Time.Clock (UTCTime)
-- import           Data.Time.Format (parseTime)
import           Hakyll
-- import           System.FilePath (takeBaseName, takeFileName, replaceFileName, replaceExtension)
-- import           System.Locale (defaultTimeLocale)
import           Text.Regex (mkRegex, subRegex)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match (fromList ["fonts/*", "images/*", "js/*", "favicon.ico"]) $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (\tag -> fromFilePath $ "tag/" ++ tag ++ "/index.html")

    -- Posts pages

    match "posts/*" $ do
        route $ removeExtension
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/_post.html" (postWithTagsCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postWithTagsCtx tags)

    -- Tags pages

    create ["tags/index.html"] $ do
        route idRoute
        compile $ do
            t <- renderTags
                (\tag url count minCount maxCount ->
                    "<a href=\"/tag/" ++ tag ++ "/\" title=\"" ++ (countText count "пост" "поста" "постов") ++
                    "\" class=\"weight-" ++ (show $ getWeight minCount maxCount count) ++ "\">" ++ tag ++ "</a>")
                (intercalate " ") tags
            makeItem t
                >>= loadAndApplyTemplate "templates/_tags-wrapper.html" postCtx
                >>= loadAndApplyTemplate "templates/_post-without-footer.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx

    tagsRules tags $ \tag identifiers -> do
        paginate <- buildPaginateWith' 5 (getTagIdent tag) identifiers
        paginateRules paginate $ \page ids -> do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                let postsCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "navlinkolder" "" `mappend`
                        constField "navlinknewer" "" `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/list.html" postsCtx


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx


    {-
    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll ("posts/*" .&&. hasVersion "list")

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            pandocCompiler
                >>= loadAndApplyTemplate "templates/_post-without-footer.html" postCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx -}

    paginate <- buildPaginateWith' 5 getPageIdent ("posts/*")
    paginateRules paginate $ \page ids -> do
        create [paginateMakeId paginate page] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                let postsCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "navlinkolder" "" `mappend`
                        constField "navlinknewer" "" `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/list.html" postsCtx


    {-
    paginate 5 $ \index maxIndex itemsForPage -> do
        let id = fromFilePath $ "page/" ++ (show index) ++ "/index.html"
        if index == 1 then return ()
        else
            create [id] $ do
                route idRoute
                compile $ do
                    let allCtx = defaultContext
                    items <- mapM (\item -> loadSnapshot item "content") itemsForPage
                    let postsCtx =
                            listField "posts" postCtx (return items) `mappend`
                            constField "navlinkolder" (getPrevNavLink index maxIndex) `mappend`
                            constField "navlinknewer" (getNextNavLink index maxIndex) `mappend`
                            defaultContext

                    makeItem ""
                        >>= loadAndApplyTemplate "templates/list.html" postsCtx
                        -}

    match (fromList ["about.md", "shoutbox.md"]) $ do
        route $ removeExtension
        compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/_post-without-footer.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

getTagIdent :: String -> PageNumber -> Identifier
getTagIdent tag pageNum
    | pageNum == 1 = fromFilePath $ "tag/" ++ tag ++ "/index.html"
    | otherwise = fromFilePath $ "tag/" ++ tag ++ "/page/" ++ (show pageNum) ++ "/index.html"

getPageIdent :: PageNumber -> Identifier
getPageIdent pageNum
    | pageNum == 1 = fromFilePath $ "index.html"
    | otherwise = fromFilePath $ "page/" ++ (show pageNum) ++ "/index.html"

postWithTagsCtx :: Tags -> Context String
postWithTagsCtx tags = tagsField "tags" tags `mappend`
    postCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    field "url" (return . identifierToUrl . toFilePath . itemIdentifier) `mappend`
    defaultContext

getPrevNavLink :: Int -> Int -> String
getPrevNavLink index maxIndex
    | index >= maxIndex = ""
    | otherwise = "<li class=\"previous\"><a href=\"/page/" ++ (show $ index + 1) ++ "/\">&larr; Старше</a></li>"

getNextNavLink :: Int -> Int -> String
getNextNavLink index maxIndex
    | index == 1 = ""
    | index == 2 = "<li class=\"next\"><a href=\"/\">Моложе &rarr;</a></li>"
    | otherwise = "<li class=\"next\"><a href=\"/page/" ++ (show $ index - 1) ++ "/\">Моложе &rarr;</a></li>"

isPublished :: (MonadMetadata m) => Identifier -> m Bool
isPublished identifier = do
    published <- getMetadataField identifier "published"
    return (published /= Just "false")

-- | Transforms 'something/something.md' into 'something/something/index.html'
-- and 'something/YYYY-MM-DD-something.md' into 'something/something/index.html'
removeExtension :: Routes
removeExtension = customRoute $ removeExtension' . toFilePath

removeExtension' :: String -> String
removeExtension' filepath = subRegex (mkRegex "^(.*)\\.md$")
                                        (subRegex (mkRegex "/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$") filepath "/\\1/index.html")
                                        "\\1/index.html"

identifierToUrl :: String -> String
identifierToUrl filepath = subRegex (mkRegex "^(.*)\\.md$")
                                        (subRegex (mkRegex "/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$") filepath "/\\1/")
                                        "\\1/"

countText :: Int -> String -> String -> String -> String
countText count one two many
    | count `mod` 100 `div` 10 == 1 =
        (show count) ++ " " ++ many
    | count `mod` 10 == 1 =
        (show count) ++ " " ++ one
    | count `mod` 10 == 2 || count `mod` 10 == 3 || count `mod` 10 == 4 =
        (show count) ++ " " ++ two
    | otherwise =
        (show count) ++ " " ++ many

getWeight :: Int -> Int -> Int -> Int
getWeight minCount maxCount count =
    round ((5 * ((fromIntegral count :: Double) - fromIntegral minCount) +
        fromIntegral maxCount - fromIntegral minCount) /
        (fromIntegral maxCount - fromIntegral minCount))


buildPaginateWith' :: MonadMetadata m
                  => Int
                  -> (PageNumber -> Identifier)
                  -> Pattern
                  -> m Paginate
buildPaginateWith' n makeId pattern = do
    -- TODO filter unpublished

    metadata <- getAllMetadata pattern
    let idents         = fst $ unzip $ sortBy compareFn metadata
        pages          = flip unfoldr idents $ \xs ->
            if null xs then Nothing else Just (splitAt n xs)
        nPages         = length pages
        paginatePages' = zip [1..] pages
        pagPlaces'     =
            [(ident, idx) | (idx,ids) <- paginatePages', ident <- ids] ++
            [(makeId i, i) | i <- [1 .. nPages]]

    return $ Paginate (M.fromList paginatePages') (M.fromList pagPlaces') makeId
        (PatternDependency pattern idents)

    where
        compareFn :: (a, Metadata) -> (a, Metadata) -> Ordering
        compareFn (_, a) (_, b)
            | M.lookup "date" a == Nothing && M.lookup "date" b == Nothing = EQ
            | M.lookup "date" a == Nothing = GT
            | M.lookup "date" b == Nothing = LT
            | otherwise = compare (b M.! "date") (a M.! "date")