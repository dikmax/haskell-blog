---
title: Обновление блога
date: 2012-09-05T23:07:23+03:00
tags: haskell, javascript, блог, программирование
---

Развитие блога не прекращается ни на минуту (ну почти). На этот раз я сделал meta-теги в заголовке страницы для [Facebook](http://developers.facebook.com/docs/opengraph/objects/builtin/). Теперь, если добавить ссылку на мой блог, то в блок в Facebook попадет начало поста. Кстати, совершенно внезапно, [вконтактик](http://vk.com) тоже правильно стал отображать описание, что не может не радовать.

В очередной раз впечатлился выразительностью Хаскеля: cколько всего можно написать одной строчкой. Вот, например, [кусочек из свеженаписанного](https://github.com/dikmax/haskell-blog/blob/b481af3dfe181198065840031607fc7a9ecc365e/src/Site.hs#L149):

~~~~~haskell
getDescription :: Node -> Text
getDescription = maybe emptyDescription 
  (until (not . T.null) (\_ -> emptyDescription) . getDescription') . 
  findChild (checkMainDiv . current) . fromNode

getDescription' :: Cursor -> Text
getDescription' = cutDescription . transformDescription .
  T.intercalate " " . map nodeText . filter checkParagraph .
  maybe [] siblings . firstChild
~~~~~

Можете сравнить с примерной калькой на JavaScript:

~~~~~javascript
var getDescription_ = function (cursor) {
    var fc = firstChild(cursor)
    return cutDescription(transformDescription(
        (fc ? siblings(fc) : []).filter(checkParagraph).map(nodeText).join(" ")
    ));
}
var getDescription = function (node) {
    var mainDiv = findChild(function (cursor) {
        return checkMainDiv(current(cursor))
    }, fromNode(node));
    return mainDiv ? getDescription_(mainDiv) || emptyDescription : emptyDescription;
}
~~~~~

Хотя да, и в JS есть своя прелесть.