--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder (toByteString)
import           Control.Monad (forM_, filterM)
import           Data.List (sortBy, intercalate, unfoldr, isSuffixOf)
import qualified Data.Map as M
import           Data.Monoid (mappend, mconcat)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
-- import           Data.Time.Format (parseTime)
import           Hakyll
-- import           System.FilePath (takeBaseName, takeFileName, replaceFileName, replaceExtension)
import           System.Locale
import           Text.HTML.TagSoup (Tag(..))
import           Text.Pandoc
import           Text.Printf (printf)
import           Text.Regex (mkRegex, subRegex)
import           Text.XmlHtml
import           XmlHtmlWriter

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "less/*.less" $ do
        compile getResourceBody

    d <- makePatternDependency "less/**"
    rulesExtraDependencies [d] $ create ["css/style.css"] $ do
        route idRoute
        compile $ loadBody "less/style.less"
            >>= makeItem
            >>= withItemBody
              (unixFilter "lessc" ["--yui-compress","-O2", "--include-path=less","-"])

    tags <- buildTags "posts/*" (\tag -> fromFilePath $ "tag/" ++ tag ++ "/index.html")

    -- Posts pages

    match "posts/*" $ do
        route $ removeExtension
        compile $ pandocCompiler'
            >>= transformPost
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/_post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" (pageCtx defaultMetadata)

    -- Tags pages

    create ["tags/index.html"] $ do
        route idRoute
        compile $ do
            t <- renderTags
                (\tag url count minCount maxCount ->
                    "<a href=\"/tag/" ++ tag ++ "/\" title=\"" ++ (countText count "пост" "поста" "постов") ++
                    "\" class=\"weight-" ++ (show $ getWeight minCount maxCount count) ++ "\">" ++ tag ++ "</a>")
                (intercalate " ") tags
            let ctx = pageCtx defaultMetadata
            makeItem t
                >>= loadAndApplyTemplate "templates/_tags-wrapper.html" ctx
                >>= loadAndApplyTemplate "templates/_post-without-footer.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    tagsRules tags $ \tag identifiers -> do
        paginate <- buildPaginateWith' 5 (getTagIdent tag) identifiers
        paginateRules paginate $ \page ids -> do
            route addIndexRoute
            compile $ do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                let postsCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        paginateContext' paginate `mappend`
                        pageCtx defaultMetadata

                makeItem ""
                    >>= loadAndApplyTemplate "templates/list.html" postsCtx


    create ["archive.html"] $ do -- TODO
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


    match "index.md" $ do
        compile $ do
            pandocCompiler'
                >>= loadAndApplyTemplate "templates/_post-without-footer.html" postCtx

    paginate <- buildPaginateWith' 5 getPageIdent ("posts/*")
    paginateRules paginate $ \page ids -> do
        route addIndexRoute
        if page == 1
            then compile $ do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                topPost <- loadBody "index.md"
                let postsCtx =
                        constField "body" topPost `mappend`
                        listField "posts" postCtx (return posts) `mappend`
                        paginateContext' paginate `mappend`
                        pageCtx defaultMetadata
                makeItem ""
                    >>= loadAndApplyTemplate "templates/index.html" postsCtx
            else compile $ do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                let postsCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        paginateContext paginate `mappend`
                        pageCtx defaultMetadata
                makeItem ""
                    >>= loadAndApplyTemplate "templates/list.html" postsCtx

    match (fromList ["about.md", "shoutbox.md"]) $ do
        route $ removeExtension
        compile $ pandocCompiler'
                >>= loadAndApplyTemplate "templates/_post-without-footer.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" (pageCtx defaultMetadata)

    -- Render RSS feed
    create ["rss"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss feedConfiguration feedCtx

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

--
-- Metadata processing
--

data FacebookType = FacebookBlog
    | FacebookArticle UTCTime [String] (Maybe String) -- Published, keywords, image
    | FacebookProfile
    | FacebookNothing

data PageMetadata = PageMetadata
    { metaTitle :: Maybe String
    , metaUrl :: String
    , metaDescription :: String
    , metaKeywords :: [String]
    , metaType :: FacebookType
    }

defaultMetadata :: PageMetadata
defaultMetadata = PageMetadata
    { metaTitle = Nothing
    , metaUrl = ""
    , metaDescription = ""
    , metaKeywords = []
    , metaType = FacebookNothing
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "[dikmax's blog]"
    , feedDescription = "Мой персональный блог"
    , feedAuthorName = "Максим Дикун"
    , feedAuthorEmail = "me@dikmax.name"
    , feedRoot = "http://dikmax.name"
    }

feedCtx = bodyField "description" `mappend` defaultContext

transformPost :: Item String -> Compiler (Item String)
transformPost item = return $ item { itemBody = demoteHeaders $ itemBody item }


getTagIdent :: String -> PageNumber -> Identifier
getTagIdent tag pageNum
    | pageNum == 1 = fromFilePath $ "tag/" ++ tag ++ "/"
    | otherwise = fromFilePath $ "tag/" ++ tag ++ "/page/" ++ (show pageNum) ++ "/"

getPageIdent :: PageNumber -> Identifier
getPageIdent pageNum
    | pageNum == 1 = fromFilePath $ ""
    | otherwise = fromFilePath $ "page/" ++ (show pageNum) ++ "/"

addIndexRoute = customRoute (\id ->
    if toFilePath id == ""
        then "index.html"
        else (toFilePath id) ++ "/index.html")

tagsContext :: Context a
tagsContext = field "tags" convertTags
    where
        convertTags item = do
            tags <- getTags $ itemIdentifier item
            return $ concat $ map (\tag -> "<a href=\"/tag/" ++ tag ++ "/\" class=\"label label-default\">" ++ tag ++ "</a> ") tags

timeLocale :: TimeLocale
timeLocale = defaultTimeLocale
  { wDays =
    [ ("Воскресенье", "вс")
    , ("Понедельник", "пн")
    , ("Вторник", "вт")
    , ("Среда", "ср")
    , ("Четверг", "чт")
    , ("Пятница", "пт")
    , ("Суббота", "сб")
    ]
  , months =
    [ ("января", "янв")
    , ("февраля", "фев")
    , ("марта", "мар")
    , ("апреля", "апр")
    , ("мая", "май")
    , ("июня", "июн")
    , ("июля", "июл")
    , ("августа", "авг")
    , ("сентября", "сен")
    , ("октября", "окт")
    , ("ноября", "ноя")
    , ("декабря", "дек")
    ]
  }


postCtx :: Context String
postCtx =
    dateFieldWith timeLocale "date" "%A, %e %B %Y, %R" `mappend`
    field "url" (return . identifierToUrl . toFilePath . itemIdentifier) `mappend`
    tagsContext `mappend`
    defaultContext

pageCtx :: PageMetadata -> Context String
pageCtx (PageMetadata title url description keywords fType)=
    constField "meta.title" (metaTitle title) `mappend`
    defaultContext
    where
        metaTitle Nothing = "[dikmax's blog]"
        metaTitle (Just title) = title ++ " :: [dikmax's blog]"

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


-- Updated versions of library functions

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

--------------------------------------------------------------------------------
-- | Takes first, current, last page and produces index of next page
type RelPage = PageNumber -> PageNumber -> PageNumber -> Maybe PageNumber

paginateField :: Paginate -> String -> RelPage -> Context a
paginateField pag fieldName relPage = field fieldName $ \item ->
    let identifier = itemIdentifier item
    in case M.lookup identifier (paginatePlaces pag) of
        Nothing -> fail $ printf
            "Hakyll.Web.Paginate: there is no page %s in paginator map."
            (show identifier)
        Just pos -> case relPage 1 pos nPages of
            Nothing   -> fail "Hakyll.Web.Paginate: No page here."
            Just pos' -> do
                let nextId = paginateMakeId pag pos'
                mroute <- getRoute nextId
                case mroute of
                    Nothing -> fail $ printf
                        "Hakyll.Web.Paginate: unable to get route for %s."
                        (show nextId)
                    Just rt -> return $ removeIndex $ toUrl rt
  where
    nPages = M.size (paginatePages pag)
    removeIndex url
        | "index.html" `isSuffixOf` url = take (length url - 10) url
        | otherwise = url

paginateContext' :: Paginate -> Context a
paginateContext' pag = mconcat
    [ paginateField pag "firstPage"
        (\f c _ -> if c <= f then Nothing else Just f)
    , paginateField pag "previousPage"
        (\f c _ -> if c <= f then Nothing else Just (c - 1))
    , paginateField pag "nextPage"
        (\_ c l -> if c >= l then Nothing else Just (c + 1))
    , paginateField pag "lastPage"
        (\_ c l -> if c >= l then Nothing else Just l)
    ]

pandocCompiler' :: Compiler (Item String)
pandocCompiler' = do
    post <- getResourceBody
    makeItem $ T.unpack $ T.decodeUtf8 $ toByteString $ renderHtmlFragment UTF8 $ writeXmlHtml defaultXmlHtmlWriterOptions
        { idPrefix = "" --postUrl post
        , debugOutput = False
        }
        (readMarkdown readerOptions $ itemBody post)

readerOptions :: ReaderOptions
readerOptions = def
  { readerSmart = True
  , readerParseRaw = True
  }

