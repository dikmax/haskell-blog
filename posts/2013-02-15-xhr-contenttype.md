---
title: XMLHttpRequest, Content-Type и Content-Length: 0
date: 2013-02-15T10:43:05+03:00
tags: firefox, google chrome, javascript, rest, программирование, работа
---

Когда в из JavaScript отправляешься AJAX запрос[^1], ставишь ему заголовок `Content-Type: application/json`, но при этом оставляешь само тело запроса пустым, то браузер самостоятельно заменяет `Content-Type` на тот, который кажется ему более подходящим. Причем для Google Chrome это `application/xml`, а для Firefox — `plain/text`. А сервер смотрит на это безобразие и отвечает — *415 Unsupported Media Type*, у него-то стоит проверка на заголовок `Content-Type`.

Как вариант решения отправлять пустой объект `{}`. Ну или поправить сервер, если есть такая возможность.

[^1]: DELETE в моем случае
