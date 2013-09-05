---
title: Получение бинарных данных в Google Closure Library
date: 2012-08-30T11:35:55+03:00
tags: closure library, javascript, программирование, работа
---

Делюсь с вами кусочком из своей практики, который может кому-нибудь пригодиться. Лично я был бы рад найти подобный пост на просторах интернета, когда столкнулся с этой проблемой.

Итак, задача: с помощью XmlHttpRequest получать бинарные данные с сервера. Ну как-то так получилось, что нужно скачивать сжатый с помощью gzip xml-файл с сервера и выбирать из него некоторый набор данных. 

Первая важная вещь, которая понадобится, --- это метод `overrideMimeType` у объекта XMLHttpRequest. Этот метод позволяет переопределить тип получаемых с сервера данных. И даже существует специальная кодировка, которая специально была задумана для приема бинарных данных --- `x-user-defined` (мы помним, что XMLHttpRequest автоматически преобразует все принятые данные в юникод). Поэтому простое добавление следующей строчки должно сделать большую часть работы.

~~~~~javascript
xhr.overrideMimeType('text/plain; charset=x-user-defined');
~~~~~

Мозг, натренированный jQuery, выдает кусок кода практически сразу:

~~~~~javascript
$.ajax({
    url: url,
    beforeSend: function(xhr) {
        xhr.overrideMimeType(
            'text/plain; charset=x-user-defined'
        );
    },
    success: successHandler,
    error: errorHandler
});
~~~~~

А вот с [Google Closure Library](https://developers.google.com/closure/library/) не все так просто. Там нет никаких `beforeSend`, и если [посмотреть в документацию](http://closure-library.googlecode.com/svn/docs/class_goog_net_XhrIo.html), то нужная функция выглядит следующим образом:

~~~~~javascript
goog.net.XhrIo.send(url, opt_callback, opt_method, opt_content, opt_headers, opt_timeoutInterval)
~~~~~

Только один callback, который срабатывает на complete, данные, загаловки, таймаут. Не густо. Но если посмотреть в код этой функции, то можно обнаружить, что на самом деле это прокси для объекта `goog.net.XhrIo`. Но `goog.net.XhrIo` тоже не конструирует XMLHttpRequest. Вместо этого использует фабрику для создания запросов, что, кстати, логично, но больше Java-way чем JS-way. Ну да ладно. Получается, чтобы вклинить вызов `overrideMimeType`, нужно сделать свою фабрику ~~с блекджеком~~, например, вот так:

~~~~~javascript
my.net.BinaryXmlHttpFactory = function () {
    goog.base(this);
};
goog.inherits(my.net.BinaryXmlHttpFactory, goog.net.DefaultXmlHttpFactory);

my.net.BinaryXmlHttpFactory.prototype.createInstance = function () {
    var xhr = goog.base(this, "createInstance");
    xhr.overrideMimeType('text/plain; charset=x-user-defined');
    return xhr;
};
~~~~~

Теперь дело осталось за малым: инициализировать `goog.net.XhrIo` c новой фабрикой и выполнить запрос:

~~~~~javascript
var xhrIo = new goog.net.XhrIo(new my.net.BinaryXmlHttpFactory());
goog.events.listen(xhrIo, goog.net.EventType.SUCCESS, successHandler);
goog.events.listen(xhrIo, [goog.net.EventType.ERROR, goog.net.EventType.ABORT], errorHandler);
xhrIo.send(url);
~~~~~

Вот и все. Сразу оговорюсь, что все это не будет работать в Internet Explorer, т.к. там нужна [своя особая магия](http://stackoverflow.com/a/5913807/682727) для получения бинарных данных, в отличие от остальных, где остаток действий выглядит довольно просто:

~~~~~javascript
var res = '';
for (var i = 0; i < data.length; ++i) {
    var code = data.charCodeAt(i);
    res += String.fromCharCode(code & 0xff);
}
~~~~~

Надеюсь, что кому-то этот пост поможет и направит мысли в правильную сторону.