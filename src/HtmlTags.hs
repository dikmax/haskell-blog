{-# LANGUAGE OverloadedStrings #-}

module HtmlTags where

import Data.Text (Text)
import Text.XmlHtml (Node(..))
import Prelude hiding (div)

infixl 0 <@, <#, <&, <&&

-- |
-- Set attribute
--
(<@) :: Node -> (Text, Text) -> Node
(Element tag attrs children) <@ attr = Element tag (attr : attrs) children
node <@ _ = node

-- |
-- Append text node
(<#) :: Node -> Text -> Node
(Element tag attrs children) <# text = Element tag attrs (children ++ [TextNode text])
node <# _ = node

-- |
-- Append any node
(<&) :: Node -> Node -> Node
(Element tag attrs children) <& node = Element tag attrs (children ++ [node])
node <& _ = node

-- |
-- Append list of nodes
(<&&) :: Node -> [Node] -> Node
(Element tag attrs children) <&& nodes = Element tag attrs (children ++ nodes)
node <&& _ = node

--
-- a element
--
a :: Node
a = Element "a" [] []

--
-- article element
--
article :: Node
article = Element "article" [] []

--
-- div element
--
div :: Node
div = Element "div" [] []

--
-- li element
--
li :: Node
li = Element "li" [] []

--
-- link element
--
link :: Node
link = Element "link" [] []

--
-- meta element
--
meta :: Node
meta = Element "meta" [] []

--
-- ul element
--
ul :: Node
ul = Element "ul" [] []
