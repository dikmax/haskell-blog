{-# LANGUAGE OverloadedStrings #-}

module XmlHtmlWriter 
  ( XmlHtmlWriterOptions(..)
  , defaultXmlHtmlWriterOptions
  , writeXmlHtml 
  ) where

import Blaze.ByteString.Builder (toByteString)
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.XmlHtml hiding (render)
import Text.Pandoc

data XmlHtmlWriterOptions = XmlHtmlWriterOptions 
  { idPrefix :: T.Text
  , debugOutput :: Bool
  , renderForRSS :: Bool
  }

defaultXmlHtmlWriterOptions :: XmlHtmlWriterOptions
defaultXmlHtmlWriterOptions = XmlHtmlWriterOptions 
  { idPrefix = ""
  , debugOutput = False
  , renderForRSS = False
  }

data WriterStateData = WriterStateData 
  { notesList :: [[Node]]
  , rawData :: T.Text
  , rawInline :: T.Text
  , writerOptions :: XmlHtmlWriterOptions
  }

type WriterState = State WriterStateData

writeXmlHtml :: XmlHtmlWriterOptions -> Pandoc -> [Node]
writeXmlHtml options pandoc 
  | debugOutput options = 
    [ Element "pre" [] 
      [ TextNode $ T.pack $ writeNative def pandoc]
    ]
  | otherwise = evalState (writeXmlHtml' pandoc) emptyState { writerOptions = updateOptions }
  where
    updateOptions = options 
      { idPrefix = if idPrefix options == "" then "new" else idPrefix options 
      }

emptyState :: WriterStateData
emptyState = WriterStateData
  { notesList = []
  , rawData = ""
  , rawInline = ""
  , writerOptions = defaultXmlHtmlWriterOptions
  }

writeXmlHtml' :: Pandoc -> WriterState [Node]
writeXmlHtml' (Pandoc _ blocks) = do
  mainBlocks <- concatBlocks blocks
  footerBlocks <- getFooter
  return $ mainBlocks ++ footerBlocks

parseRawString :: T.Text -> [Node]
parseRawString raw
  | raw == "" = []
  | otherwise = either (\ a -> [TextNode $ T.pack a]) extractData $ parseHTML "raw" $ T.encodeUtf8 raw
  where 
    extractData (HtmlDocument _ _ content) = content
    extractData (XmlDocument _ _ content) = content

concatBlocks :: [Block] -> WriterState [Node]
concatBlocks blocks = do
  result <- foldM concatBlocks' [] blocks
  writerState <- get
  put writerState {rawData = ""}
  return $ result ++ parseRawString (rawData writerState)

concatBlocks' :: [Node] -> Block -> WriterState [Node]
concatBlocks' nodes block@(Plain _) = do
  items <- writeBlock block
  return (nodes ++ items)
concatBlocks' nodes block@(RawBlock _ _) = do
  items <- writeBlock block
  return (nodes ++ items)
concatBlocks' nodes block = do
  writerState <- get
  put writerState {rawData = ""}
  items <- writeBlock block
  return (nodes ++ parseRawString (rawData writerState) ++ items)

writeBlock :: Block -> WriterState [Node]
writeBlock (Plain inline) = do
  inlines <- concatInlines inline
  modify (\s -> s { rawData = rawData s `T.append` T.decodeUtf8 (toByteString $ renderHtmlFragment UTF8 inlines) } )
  return []
writeBlock (Para inline) = do
  inlines <- concatInlines inline
  return [Element "p" [] inlines]
writeBlock (CodeBlock (identifier, classes, others) code) = return 
  [ Element "pre" mapAttrs 
    [ Element "code" mapAttrs 
      [ TextNode $ T.pack code ]
    ]
  ]
  where 
    mapAttrs = writeAttr (identifier, "sourceCode" : classes, others) 
writeBlock (RawBlock "html" str) = do  
  modify (\s -> s { rawData = rawData s `T.append` T.pack str })
  return []
writeBlock (RawBlock _ _) = return []
writeBlock (BlockQuote blocks) = do
  items <- concatBlocks blocks
  return [ Element "blockquote" [] items ]
writeBlock (OrderedList (startNum, numStyle, _) listItems) = do
  items <- foldM processListItems [] listItems
  return [ Element "ol" 
    ( ("type", case numStyle of
        Decimal    -> "1"
        LowerAlpha -> "a"
        UpperAlpha -> "A"
        LowerRoman -> "i"
        UpperRoman -> "I"
        _          -> "1"
      ) :
      [ ("start", T.pack $ show startNum) | startNum /= 1 ]
    )
    items ]  
writeBlock (BulletList listItems) = do
  items <- foldM processListItems [] listItems
  return [ Element "ul" [] items ]
writeBlock (DefinitionList _) = return [TextNode "DefinitionList not implemented"]
writeBlock (Header 1 _ inline) = do  -- TODO second header parameter
  inlines <- concatInlines inline
  return [Element "h2" [] inlines]
writeBlock (Header 2 _ inline) = do
  inlines <- concatInlines inline
  return [Element "h3" [] inlines]
writeBlock (Header 3 _ inline) = do
  inlines <- concatInlines inline
  return [Element "h4" [] inlines]
writeBlock (Header 4 _ inline) = do
  inlines <- concatInlines inline
  return [Element "h5" [] inlines]
writeBlock (Header _ _ inline) = do
  inlines <- concatInlines inline
  return [Element "h6" [] inlines]
writeBlock HorizontalRule = return [Element "hr" [] []]
writeBlock (Table {}) = return [TextNode "Table not implemented"]
writeBlock Null = return []

processListItems :: [Node] -> [Block] -> WriterState [Node]
processListItems nodes blocks = do
  items <- concatBlocks blocks
  return (nodes ++ [Element "li" [] items])

writeAttr :: Attr -> [(T.Text, T.Text)]
writeAttr (identifier, classes, others) = 
  [("id", T.pack identifier) | identifier /= ""] ++
  [("class", classesString) | classesString /= ""] ++
  map (\(k, v) -> (T.pack k, T.pack v)) others
  where 
    classesString = T.intercalate " " $ map T.pack classes

concatInlines :: [Inline] -> WriterState [Node]
concatInlines inlines = do
  result <- foldM concatInlines' [] inlines
  writerState <- get
  put writerState {rawInline = ""}
  return $ result ++ parseRawString (rawInline writerState)

concatInlines' :: [Node] -> Inline -> WriterState [Node]
concatInlines' nodes inline = do
  writerState <- get
  if rawInline writerState == ""
    then do
      items <- writeInline inline
      return (nodes ++ items)
    else do
      str <- writeRawInline inline
      put writerState 
        { rawInline = rawInline writerState `T.append` str
        }
      return nodes

writeInline :: Inline -> WriterState [Node]
writeInline (Str string) = return [TextNode $ T.pack string]
writeInline (Emph inline) = do
  inlines <- concatInlines inline
  return [Element "em" [] inlines]
writeInline (Strong inline) = do
  inlines <- concatInlines inline
  return [Element "strong" [] inlines]
writeInline (Strikeout inline) = do
  inlines <- concatInlines inline
  return [Element "s" [] inlines]
writeInline (Superscript inline) = do
  inlines <- concatInlines inline
  return [Element "sup" [] inlines]
writeInline (Subscript inline) = do
  inlines <- concatInlines inline
  return [Element "sub" [] inlines]
writeInline (SmallCaps inline) = do
  inlines <- concatInlines inline
  return [Element "span" [("style", "font-variant: small-caps;")] inlines]
writeInline (Quoted SingleQuote inline) = do
  inlines <- concatInlines inline
  return $ [TextNode "'"] ++ inlines ++ [TextNode "'"]
writeInline (Quoted DoubleQuote inline) = do
  inlines <- concatInlines inline
  return $ [TextNode "«"] ++ inlines ++ [TextNode "»"]
writeInline (Cite _ _) = return [TextNode "Cite not implemented"]
writeInline (Code attr code) = return 
  [ Element "code" (writeAttr attr) 
    [ TextNode $ T.pack code ]
  ]
writeInline Space = return [TextNode " "]
writeInline LineBreak = return [Element "br" [] []]
writeInline (Math InlineMath str) = return
  [ Element "span" [("class", "math")]
    [ TextNode $ "\\(" `T.append` (T.pack str) `T.append` "\\)" ]
  ]
writeInline (Math DisplayMath str) = return
  [ Element "span" [("class", "math")]
    [ TextNode $ "\\[" `T.append` (T.pack str) `T.append` "\\]" ]
  ]
writeInline (RawInline "html" str) = do
  modify (\s -> s {rawInline = rawInline s `T.append` T.pack str})
  return []
writeInline (RawInline _ _) = return []
writeInline (Link inline target) = do
  inlines <- concatInlines inline
  return [
    Element "a" 
      [("href", T.pack $ fst target), ("title", T.pack $ snd target)] inlines]
  --writeInline (Image _ _) = [TextNode "Image not implemented"]
writeInline (Image inline target) = do
  inlines <- concatInlines inline
  case "http://www.youtube.com/watch?v=" `T.isPrefixOf` T.pack (fst target) of
    True ->
      return
        [ Element "div" [("class", "figure")]
          ([ Element "p" [("class", "figure-description")] inlines | inline /= []] ++
          [ Element "iframe"
            [ ("width", "560")
            , ("height", "315")
            , ("src", "http://www.youtube.com/embed/" `T.append`
                (videoId $ T.pack $ fst target) `T.append` "?wmode=transparent")
            , ("frameborder", "0")
            , ("allowfullscreen", "allowfullscreen")
            , ("class", "img-polaroid")
            ] []
          ])
        ]
    False ->
      return
        [ Element "div" [("class", "figure")]
          ([ Element "p" [("class", "figure-description")] inlines | inline /= []] ++
          [ Element "img"
            [ ("src", T.pack $ fst target)
            , ("title", T.pack $ snd target)
            , ("alt", T.pack $ snd target)
            , ("class", "img-polaroid")
            ] []
          ])
        ]
  where
    videoId url = T.takeWhile (/= '&') $ T.replace "http://www.youtube.com/watch?v=" "" url
writeInline (Note block) = do
  blocks <- concatBlocks block
  writerState <- get
  put writerState {
    notesList = notesList writerState ++ [blocks]
  }
  let noteId = length (notesList writerState) + 1
  return 
    [ Element "sup" 
      [ ("id", "note-" `T.append` idPrefix (writerOptions writerState) `T.append` T.pack (show noteId))
      , ("class", "note-link") 
      ] 
      [ TextNode $ T.pack $ show noteId ]
    ]

concatRawInlines :: [Inline] -> WriterState T.Text
concatRawInlines inlines = do
  writerState <- get
  put writerState {rawInline = ""}  
  result <- foldM concatRawInlines' "" inlines
  put writerState {rawInline = rawInline writerState}
  return result

concatRawInlines' :: T.Text -> Inline -> WriterState T.Text
concatRawInlines' text inline = do
  str <- writeRawInline inline
  return $ text `T.append` str

writeRawAttr :: Attr -> T.Text
writeRawAttr attr = 
  -- TODO value escaping
  T.intercalate " " $ map (\(k, v) -> k `T.append` "=\"" `T.append` v `T.append` "\"") $ writeAttr attr

writeRawInline :: Inline -> WriterState T.Text
writeRawInline (Str string) = return $ T.pack string
writeRawInline (Emph inline) = do
  inlines <- concatRawInlines inline
  return ("<em>" `T.append` inlines `T.append` "</em>")
writeRawInline (Strong inline) = do
  inlines <- concatRawInlines inline
  return ("<strong>" `T.append` inlines `T.append` "</strong>")
writeRawInline (Strikeout inline) = do
  inlines <- concatRawInlines inline
  return ("<s>" `T.append` inlines `T.append` "</s>")
writeRawInline (Superscript inline) = do
  inlines <- concatRawInlines inline
  return ("<sup>" `T.append` inlines `T.append` "</sup>")
writeRawInline (Subscript inline) = do
  inlines <- concatRawInlines inline
  return ("<sub>" `T.append` inlines `T.append` "</sub>")
writeRawInline (SmallCaps inline) = do
  inlines <- concatRawInlines inline
  return ("<span style=\"font-variant: small-caps;\">" `T.append` inlines `T.append` "</span>")
writeRawInline (Quoted SingleQuote inline) = do
  inlines <- concatRawInlines inline
  return ("'" `T.append` inlines `T.append` "'")
writeRawInline (Quoted DoubleQuote inline) = do
  inlines <- concatRawInlines inline
  return ("«" `T.append` inlines `T.append` "»")
writeRawInline (Cite _ _) = return "Cite not implemented"
writeRawInline (Code attr code) = 
  return ("<code " `T.append` writeRawAttr attr `T.append` ">" `T.append` T.pack code `T.append` "</code>" )
writeRawInline Space = return " "
writeRawInline LineBreak = return "<br />"
writeRawInline (Math _ _) = return "Math not implemented"
writeRawInline (RawInline "html" str) = return $ T.pack str
writeRawInline (RawInline _ _) = return ""
writeRawInline (Link inline target) = do
  inlines <- concatRawInlines inline
  return ("<a " `T.append` writeRawAttr ("", [], [("href", fst target), ("title", snd target)]) `T.append` ">"
    `T.append` inlines `T.append` "</a>" )
  --writeInline (Image _ _) = [TextNode "Image not implemented"]
writeRawInline (Image inline target) = do
  inlines <- concatRawInlines inline
  return ("<div class=\"\">" `T.append`
    (if inline /= [] then "<p class=\"figure-description\">" `T.append` inlines `T.append` "</p>" else "")
    `T.append` "<img " `T.append`
    writeRawAttr ("", [], [ ("src", fst target)
        , ("title", snd target)
        , ("alt", snd target)
        , ("class", "img-polaroid")
        ]) `T.append` " />"
    )
writeRawInline (Note block) = do
  blocks <- concatBlocks block
  writerState <- get
  put writerState {
    notesList = notesList writerState ++ [blocks]
  }
  let noteId = length (notesList writerState) + 1
  return 
    ("<sup id=\"node-" `T.append` idPrefix (writerOptions writerState) `T.append` T.pack (show noteId) `T.append`
      "\" class=\"note-link\">" `T.append` T.pack (show noteId) `T.append` "</sup>")

getFooter :: WriterState [Node]
getFooter = do
  writerState <- get
  return
    [ Element "div" [ ("class", "footnotes") ]
      [ Element "hr" [] []
      , Element "ol" [] $
        transformNotes (notesList writerState) 1 ("note-" `T.append` idPrefix (writerOptions writerState))
      ] | length (notesList writerState) > 0
    ]
  where
    transformNotes :: [[Node]] -> Int -> T.Text -> [Node]
    transformNotes (n:ns) i prefix = 
      Element "li" [("data-for", prefix `T.append` T.pack (show i))] n
      : transformNotes ns (i+1) prefix
    transformNotes [] _ _ = []


