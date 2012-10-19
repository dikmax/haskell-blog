{-# LANGUAGE OverloadedStrings #-}

module HtmlAttributes where

import Data.Text (Text)

alt :: Text -> (Text, Text)
alt attribute = ("alt", attribute)

-- Use <. operator
cls :: Text -> (Text, Text)
cls attribute = ("class", attribute)

content :: Text -> (Text, Text)
content attribute = ("content", attribute)

datetime :: Text -> (Text, Text)
datetime attribute = ("datetime", attribute)

href :: Text -> (Text, Text)
href attribute = ("href", attribute)

itemprop :: Text -> (Text, Text)
itemprop attribute = ("itemprop", attribute)

itemscope :: Text -> (Text, Text)
itemscope attribute = ("itemscope", attribute)

itemtype :: Text -> (Text, Text)
itemtype attribute = ("itemtype", attribute)

src :: Text -> (Text, Text)
src attribute = ("src", attribute)

style :: Text -> (Text, Text)
style attribute = ("style", attribute)

type_ :: Text -> (Text, Text)
type_ attribute = ("type", attribute)

title :: Text -> (Text, Text)
title attribute = ("title", attribute)

typeJavascript :: (Text, Text)
typeJavascript = type_ "text/javascript"