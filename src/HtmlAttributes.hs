{-# LANGUAGE OverloadedStrings #-}

module HtmlAttributes where

import Data.Text (Text)

class_ :: Text -> (Text, Text)
class_ attribute = ("class", attribute)

content :: Text -> (Text, Text)
content attribute = ("content", attribute)

href :: Text -> (Text, Text)
href attribute = ("href", attribute)

itemprop :: Text -> (Text, Text)
itemprop attribute = ("itemprop", attribute)

itemscope :: Text -> (Text, Text)
itemscope attribute = ("itemscope", attribute)

itemtype :: Text -> (Text, Text)
itemtype attribute = ("itemtype", attribute)
