--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_, zipWithM_, liftM, filterM)
import           Data.List (sortBy, intercalate)
import           Data.Monoid (mappend)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (parseTime)
import           Hakyll
import           Hakyll.Web.Tags
import           System.FilePath (takeBaseName, takeFileName, replaceFileName, replaceExtension)
import           System.Locale (defaultTimeLocale)
import           Text.Regex (mkRegex, subRegex)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    tags <- buildTags "posts/*" (\tag -> fromFilePath $ "tag/" ++ tag ++ "/index.html")

    {-match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls-}

    matchPosts $ \identifier ->
        create [identifier] $ do
            route $ removeExtension
            compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/_post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["tags/index.html"] $ do
        route idRoute
        compile $ do
            t <- renderTags
                (\tag url count minCount maxCount ->
                    "<a href=\"/tag/" ++ tag ++ "/\" title=\"" ++ (countText count "пост" "поста" "постов") ++ "\">" ++ tag ++ "</a>")
                (intercalate " ")
            makeItem t
                >>= loadAndApplyTemplate "templates/_post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx



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
                >>= relativizeUrls


    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAllSnapshots "posts/*" "content"

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            pandocCompiler
                >>= loadAndApplyTemplate "templates/_post-without-footer.html" postCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= relativizeUrls

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

    match (fromList ["about.md", "shoutbox.md"]) $ do
        route $ removeExtension
        compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/_post-without-footer.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
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

--------------------------------------------------------------------------------
-- | Split list into equal sized sublists.
-- https://github.com/ian-ross/blog
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
    where (ys,zs) = splitAt n xs

paginate:: Int -> (Int -> Int -> [Identifier] -> Rules ()) -> Rules ()
paginate itemsPerPage rules = do
    identifiers <- getMatches "posts/*"
    items <- filterM isPublished identifiers
    let sorted = reverse $ sortBy byDate items
        chunks = chunk itemsPerPage sorted
        maxIndex = length chunks
        pageNumbers = take maxIndex [1..]
        process i is = rules i maxIndex is
    zipWithM_ process pageNumbers chunks
        where
            byDate id1 id2 =
                let fn1 = takeFileName $ toFilePath id1
                    fn2 = takeFileName $ toFilePath id2
                    parseTime' fn = parseTime defaultTimeLocale "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn
                in compare ((parseTime' fn1) :: Maybe UTCTime) ((parseTime' fn2) :: Maybe UTCTime)

matchPosts :: (Identifier -> Rules ()) -> Rules ()
matchPosts process = do
    identifiers <- getMatches "posts/*"
    items <- filterM isPublished identifiers
    forM_ items process

isPublished :: (MonadMetadata m) => Identifier -> m Bool
isPublished identifier = do
    published <- getMetadataField identifier "published"
    return (published /= Just "false")

-- | Transforms 'something/something.md' into 'something/something/index.html'
-- and 'something/YYYY-MM-DD-something.md' into 'something/something/index.html'
removeExtension :: Routes
removeExtension = customRoute $
    (\filepath ->
        subRegex (mkRegex "^(.*)\\.md$")
            (subRegex (mkRegex "/[0-9]{4}-[0-9]{2}-[0-9]{2}-(.*)\\.md$") filepath "/\\1/index.html")
            "\\1/index.html") .
    toFilePath

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
