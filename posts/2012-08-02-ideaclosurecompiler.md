---
title: Google Closure Compiler JsDoc в продуктах JetBrains
date: 2012-08-02T11:56:54+03:00
tags: closure compiler, intellij idea, javascript, программирование, работа
---

Свершилось чудо! JetBrains услышали мои молитвы и [добавили поддержку](http://blog.jetbrains.com/webide/2012/08/closure-syntax/) [аннотаций Google Closure Compiler](https://developers.google.com/closure/compiler/docs/js-for-compiler) в свои [новые](http://confluence.jetbrains.com/display/IDEADEV/IDEA+12+EAP) [продукты](http://confluence.jetbrains.com/display/WI/Web+IDE+EAP). Теперь редактор знает, чем отличается тип параметра `{string}` от `{!string}` и от `{?string}`. Он больше не ругается за то, что я пытаюсь сравнивать `{String}` cо `{string}`, ну не красота ли? Ну и куча всяких других полезных плюшек, вроде полного понимания структуры наследования объектов. Ради всего этого можно поставить EAP-версию и потерпеть другие мелкие баги.
