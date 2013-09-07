---
title: Оптимизация
date: 2012-10-01T23:00:59+03:00
tags: css, google closure, javascript, блог
---

Блог подвергся тотальной оптимизации. Теперь все должно работать еще быстрее и еще лучше. Теперь на каждой странице осталось только по одному js и css файлу, конечно, если не считать сторонние сервисы --- [Disqus](http://disqus.com/) и [Google Analytics](http://www.google.com/analytics/). И вообще везде царствует гармония и фен-шуй.

Весь javascript был переписан с использованием [Google Closurе](https://developers.google.com/closure/). Попутно в него были внесены мелкие, но приятные дополнения. Например, навигация с помощью комбинаций клавиш Ctrl+← и Ctrl+→ или эмуляция переходов между состояниями (transitions) для старых браузеров, не поддерживающих [CSS Transitions](http://www.w3.org/TR/css3-transitions/). Конечно, для этого пришлось написать часть кода из [плагинов Twitter Bootstrap](http://twitter.github.com/bootstrap/javascript.html). Но в любом случае я доволен: суммарный размер всех js фалов на рабочем сервере уменьшился в 3 раза: со 180 до 60 килобайт. Да и прокачанный навык в Google Closure многого стоит.

С CSS было проще: я просто выкинул неиспользуемые части из [Twitter Bootstrap](http://twitter.github.com/bootstrap/) и добавил свой код в результирующий файл. Это не заняло много времени.

А еще теперь при наведении на стоку в листинге с кодом слева подписывается ее номер. Не особо нужная штука, но может кому пригодиться.

Весь код, как обычно, [на GitHub](https://github.com/dikmax/haskell-blog), включая новые большие скрипты для сборки проекта. Смотрим, изучаем, комментируем.