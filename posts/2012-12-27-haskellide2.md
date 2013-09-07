---
title: Haskell IDE 2
date: 2012-12-27T18:31:45+03:00
tags: haskell, ide, jetbrains, phpstorm, программирование
---

Прошло уже довольно много времени с [прошлого обзора IDE для Haskell](/post/haskellide). Поэтому я решил рассказать, на каком варианте я в итоге остановился. Встречаем [PhpStorm](http://www.jetbrains.com/phpstorm/)! Казалось бы, причем тут специализированная среда разработки для PHP к Haskell. Но тому есть несколько причин. 

Во-первых, я работаю JavaScript/PHP разработчиком, и так уж получилось, что я привык к этому продукту от JetBrains. Во-вторых, разработка сайта — это во многом работа с JavaScript/HTML, а лучшей поддержки этих языков я нигде не встречал.

С самим Haskell в IDE все не так хорошо, как с другими языками, но поддержка синтаксиса, хотя не всегда правильная, но есть. Да и поддержка пакетов Sublime Text (и бандлов TextMate) скоро должна привнести новые краски в редактор (пока этот плагин есть только в тестовых сборках). Поэтому осталось добавить `cabal build` в External Tools, назначить горячую клавишу (у меня `Ctrl+Alt+Shift+B`) --- и можно спокойно работать.

Ну и сделанный специально для вас сделанный скриншот, наслаждайтесь:

[![PhpStorm](http://a51056ce8d9b948fb69e-8de36eb37b2366f5a76a776c3dee0b32.r42.cf1.rackcdn.com/phpstorm-small.jpg "PhpStorm")](http://a51056ce8d9b948fb69e-8de36eb37b2366f5a76a776c3dee0b32.r42.cf1.rackcdn.com/phpstorm.png)