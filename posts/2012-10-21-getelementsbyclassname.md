---
title: getElementsByClassName в IE
date: 2012-10-21T22:19:27+03:00
tags: javascript, vanillajs, кроссбраузерность, программирование
---

А вы знаете, что в Internet Explorer вплоть до 8-го у `document` нет метода `getElementsByClassName`? А следовательно, все ваши верстки, основанные только на классах, будут сильно проигрывать в производительности. Все фреймворки эмулируют метод вызовом `getElementsByTagName('*')` с дальнейшей ручной фильтрацией.

~~~~~javascript
var els = document.getElementsByTagName("*"), filtered = [], len = els.length, i;
for (i = 0; i < len; ++i) {
    el = els[i];
    el.className.search(/^(.* |)some-class( .*|)$/) !== -1 && filtered.push(el);
}
~~~~~

Ну это код на случай, если вам придется использовать [vanilla.js](http://dikmax.name/post/vanillajs). Обратите внимание, что в цикле нельзя изменять DOM-дерево. Нужно сначала скопировать все подходящие элементы в отдельный массив и только потом их обрабатывать. Все потому, что `getElementsByTagName` (как и все остальные похожие функции) возвращает `NodeList`, который динамически изменяется вместе с деревом.

May the Force be with you.