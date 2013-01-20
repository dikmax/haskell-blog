module HtmlAttributes where

import Text.Blaze.Internal (Attribute, AttributeValue, attribute)

itemscope :: AttributeValue  -- ^ Attribute value.
          -> Attribute       -- ^ Resulting attribute.
itemscope = attribute "itemscope" " itemscope=\""

itemtype :: AttributeValue  -- ^ Attribute value.
         -> Attribute       -- ^ Resulting attribute.
itemtype = attribute "itemtype" " itemtype=\""