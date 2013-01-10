{-# LANGUAGE OverloadedStrings #-}

module HtmlTags where

import Data.Text (Text)
import Text.XmlHtml (Node(..))
import Prelude hiding (div)

infixl 1 <@, <., <#
infixl 0 <&, <&&

-- |
-- Set attribute
--
(<@) :: Node -> (Text, Text) -> Node
(Element tag attrs children) <@ attr = Element tag (attr : attrs) children
node <@ _ = node

-- |
-- Set class
--
(<.) :: Node -> Text -> Node
(Element tag attrs children) <. cls = Element tag (("class", cls) : attrs) children
node <. _ = node

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
-- br element
--
br :: Node
br = Element "br" [] []

--
-- div element
--
div :: Node
div = Element "div" [] []

--
-- h1 element
--
h1 :: Node
h1 = Element "h1" [] []

--
-- h2 element
--
h2 :: Node
h2 = Element "h2" [] []

--
-- h3 element
--
h3 :: Node
h3 = Element "h3" [] []

--
-- h4 element
--
h4 :: Node
h4 = Element "h4" [] []

--
-- h5 element
--
h5 :: Node
h5 = Element "h5" [] []

--
-- h6 element
--
h6 :: Node
h6 = Element "h6" [] []

--
-- header element
--
header :: Node
header = Element "header" [] []

--
-- i element
--
i :: Node
i = Element "i" [] []

--
-- img element
--
img :: Node
img = Element "img" [] []

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
-- p element
--
p :: Node
p = Element "p" [] []

--
-- script element
--
script :: Node
script = Element "script" [] []

--
-- span element
--
span :: Node
span = Element "span" [] []

--
-- title element
--
title :: Node
title = Element "title" [] []

--
-- ul element
--
ul :: Node
ul = Element "ul" [] []
