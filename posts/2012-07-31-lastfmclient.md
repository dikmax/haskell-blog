---
title: «Настройка» официального клиента Last.fm под Linux
date: 2012-07-31T10:02:58+03:00
tags: last.fm, linux, инструкция, музыка
---

Здравствуйте, мои маленькие друзья. Сегодня мы будем настраивать [официальный клиент](http://www.last.fm/download) [Last.fm](http://www.last.fm/), чтобы он заиграл нам бесплатное персональное радио. И все это будет происходить в расово-верной операционной системе Linux, в ее наиболее популярной реинкарнации Ubuntu или Kubuntu. Я думаю, что любой Debian-based дистрибутив тоже подойдет.

Итак, во первых нам нужен сам официальный клиент для Last.fm. Открываем Software Center или консоль, кому что удобнее. Я выбираю второй способ. И устанавливаем пакет lastfm:

~~~~~bash
$ sudo apt-get install lastfm
~~~~~

Затем нам нужен hex-редактор файлов. Я как приверженец KDE поставил себе [Okteta](http://www.kde.org/applications/utilities/okteta/). Для адептов Ubuntu скорее подойдет [GHex](https://live.gnome.org/Ghex/).

~~~~~bash
$ sudo apt-get install okteta
~~~~~

А теперь начинаем делать магию. Запускаем наш hex-редактор с правами root-пользователя.

~~~~~bash
$ sudo okteta
~~~~~

Запустили? Тогда открываем файл `/usr/lib/lastfm/libLastFmTools.so.1.0.0`.

![](http://c358655.r55.cf1.rackcdn.com/lastfm1.png)

Ищем строку api_key:

![](http://c358655.r55.cf1.rackcdn.com/lastfm2.png)

Вот она:

![](http://c358655.r55.cf1.rackcdn.com/lastfm3.png)

И, наконец, заменяем первый символ `a` (код 61) на символ с кодом 00.

![](http://c358655.r55.cf1.rackcdn.com/lastfm4.png)

Сохраняем файл, закрываем редактор. Теперь клиент lastfm готов к использованию. Запускаем, вводим логин и пароль и наслаждаемся прекрасной музыкой!

![](http://c358655.r55.cf1.rackcdn.com/lastfm5.png)

P.S. Идея инструкции была позаимствована с [хабра](http://habrahabr.ru/post/145318/).

**UPD**. [Готовый шелл-скрипт](http://dikmax.name/post/lastfmclient2).