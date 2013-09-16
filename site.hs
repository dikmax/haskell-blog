--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Blaze.ByteString.Builder (toByteString)
import           Control.Monad (forM_, filterM)
import           Data.Char
import           Data.List (sortBy, intercalate, unfoldr, isSuffixOf, find)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid (mappend, mconcat)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime)
import           Hakyll
-- import           System.FilePath (takeBaseName, takeFileName, replaceFileName, replaceExtension)
import           System.Locale
import           Text.HTML.TagSoup (Tag(..))
import qualified Text.HTML.TagSoup as TS
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

        compile $ do
            identifier <- getUnderlying
            title <- getMetadataField identifier "title"
            tags <- getTags identifier
            description <- getMetadataField identifier "description"
            item <- pandocCompiler' >>= saveSnapshot "content"
            let images = map (fromMaybe "") $ filter isJust $ map imagesMap $ TS.parseTags $ itemBody item
            time <- getItemUTC defaultTimeLocale identifier
            loadAndApplyTemplate "templates/_post.html" postCtx item
                >>= loadAndApplyTemplate "templates/default.html" (pageCtx $ defaultMetadata
                    { metaTitle = title
                    , metaUrl = '/' : (identifierToUrl $ toFilePath identifier)
                    , metaKeywords = tags
                    , metaDescription = fromMaybe (cutDescription $ transformDescription $ escapeHtml $ TS.innerText $ TS.parseTags $ itemBody item) description
                    , metaType = FacebookArticle time tags images
                    })

    -- Tags pages

    create ["tags/index.html"] $ do
        route idRoute
        compile $ do
            t <- renderTags
                (\tag url count minCount maxCount ->
                    "<a href=\"/tag/" ++ tag ++ "/\" title=\"" ++ (countText count "пост" "поста" "постов") ++
                    "\" class=\"weight-" ++ (show $ getWeight minCount maxCount count) ++ "\">" ++ tag ++ "</a>")
                (intercalate " ") tags
            let ctx = pageCtx (defaultMetadata
                    { metaTitle = Just "Темы"
                    , metaDescription = "Полный список тем (тегов) на сайте"
                    , metaUrl = "/tags/"
                    })
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
                        pageCtx (defaultMetadata
                            { metaTitle =
                                if page == 1
                                    then Just $ "\"" ++ tag ++ "\""
                                    else Just $ "\"" ++ tag ++ "\", " ++ (show page) ++ "-я страница"
                            , metaDescription =
                                if page == 1
                                    then "Мой персональный блог, записи с тегом \"" ++ tag ++ "\"."
                                    else "Мой персональный блог, записи с тегом \"" ++ tag ++ "\" с "
                                        ++ (show ((page - 1) * 5 + 1)) ++ " по " ++ (show (page * 5)) ++ "."
                            , metaUrl =
                                if page == 1
                                    then "/tag/" ++ tag ++ "/"
                                    else "/tag/" ++ tag ++ "/page/" ++ (show page) ++ "/"
                            })

                makeItem ""
                    >>= loadAndApplyTemplate "templates/list.html" postsCtx

    archiveRules

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
                        pageCtx (defaultMetadata
                            { metaDescription = "Мой персональный блог. "
                                ++ "Я рассказываю о программировании и иногда о своей жизни."
                            })
                makeItem ""
                    >>= loadAndApplyTemplate "templates/index.html" postsCtx
            else compile $ do
                posts <- recentFirst =<< loadAllSnapshots ids "content"
                let postsCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        paginateContext paginate `mappend`
                        pageCtx (defaultMetadata
                            { metaTitle = Just $ (show page) ++ "-я страница"
                            , metaDescription = "Мой персональный блог, записи с " ++ (show ((page - 1) * 5 + 1))
                                ++ " по " ++ (show (page * 5)) ++ "."
                            , metaUrl = "/page/" ++ (show page) ++ "/"
                            })
                makeItem ""
                    >>= loadAndApplyTemplate "templates/list.html" postsCtx

    match (fromList ["about.md", "shoutbox.md"]) $ do
        route $ removeExtension
        compile $ do
            identifier <- getUnderlying
            title <- getMetadataField identifier "title"
            description <- getMetadataField identifier "description"
            pandocCompiler'
                >>= loadAndApplyTemplate "templates/_post-without-footer.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" (pageCtx (defaultMetadata
                    { metaTitle = title
                    , metaDescription = fromMaybe "" description
                    , metaUrl = '/' : (identifierToUrl $ toFilePath identifier)
                    }))

    -- Render RSS feed
    create ["rss"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss feedConfiguration feedCtx

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

archiveRules :: Rules ()
archiveRules = do
    ids <- getMatches "posts/*"
    years <- mapM yearsMap ids
    let ym = sortBy (\a b -> compare (fst b) (fst a)) $ yearsMap1 years
        firstYear = fst $ head ym
        fp year
            | year == firstYear = "archive/index.html"
            | otherwise = "archive/" ++ year ++ "/index.html"
        fp' year
            | year == firstYear = "/archive/"
            | otherwise = "/archive/" ++ year ++ "/"
    forM_ ym $ \(year, list) ->
        create [fromFilePath $ fp year] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAllSnapshots (fromList list) "content"
                let yearCtx =
                        field "active" (\i -> if itemBody i == year then return "active" else fail "") `mappend`
                        field "href" (\i -> return $ fp' $ itemBody i) `mappend`
                        bodyField "year"

                    archiveCtx =
                        listField "years" yearCtx (mapM (\k -> makeItem $ fst k) ym) `mappend`
                        listField "posts" postCtx (return posts) `mappend`
                        pageCtx (defaultMetadata
                            { metaTitle = Just "Архив"
                            , metaDescription = "Список всех постов для \"быстрого поиска\""
                            , metaUrl = "/archive/"
                            })
                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx

    where
        yearsMap i = do
            utc <- getItemUTC defaultTimeLocale i
            return (formatTime defaultTimeLocale "%Y" utc, [i])
        yearsMap1 = M.assocs . M.fromListWith (++)


--
-- Metadata processing
--

data FacebookType = FacebookBlog
    | FacebookArticle UTCTime [String] [String] -- Published, keywords, images
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
    , metaUrl = "/"
    , metaDescription = ""
    , metaKeywords = ["Blog", "блог"]
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

-- Replate newlines with spaces
transformDescription :: String -> String
transformDescription = map (\ch -> if ch == '\n' then ' ' else ch)

-- Cut long descriptions
cutDescription :: String -> String
cutDescription d
    | length d > 512 = reverse (dropWhile isSpace $ dropWhile (not . isSpace) $ reverse $ take 512 d) ++ "..."
    | otherwise = d

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
    constField "meta.title" (escapeHtml $ metaTitle title) `mappend`
    constField "meta.url" (escapeHtml $ "http://dikmax.name" ++ url) `mappend`
    constField "meta.description" (escapeHtml description) `mappend`
    constField "meta.keywords" (escapeHtml $ intercalate ", " keywords) `mappend`
    constField "meta.dc.subject" (escapeHtml $ intercalate "; " keywords) `mappend`
    facebookFields fType `mappend`
    defaultContext
    where
        metaTitle Nothing = "[dikmax's blog]"
        metaTitle (Just title) = title ++ " :: [dikmax's blog]"

        facebookFields (FacebookArticle published keywords images) =
                constField "meta.facebook.article" "" `mappend`
                constField "meta.facebook.published" (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" published) `mappend`
                listField "meta.facebook.tags" defaultContext (mapM (\k -> makeItem k) keywords) `mappend`
                listField "meta.facebook.images" defaultContext (mapM (\k -> makeItem k) images)
        -- TODO Facebook profile
        facebookFields _ = constField "meta.facebook.nothing" ""

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

imagesMap :: Tag String -> Maybe String
imagesMap (TagOpen "img" attrs) = fmap snd $ find (\attr -> fst attr == "src") attrs
imagesMap _ = Nothing
