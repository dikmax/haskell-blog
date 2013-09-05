---
title: Отладка событий в ExtJS/Sencha Touch
date: 2013-07-27T06:02:44+03:00
tags: extjs, javascript, sencha touch, программирование
---

Иногда, когда пишешь приложение на Sencha Touch или Ext JS, очень нужно отследить порядок событий в разлиных компонентах и передаваемые в обработчики параметры. Зачастую это помогает лучше понять, что происходит и в самом фреймворке. Мне это тоже иногда необходимо, поэтому я написал небольшой миксин, который можно встраивать в свои компоненты.

Вариант для Sencha Touch:

~~~~~javascript
Ext.define('Ext.debug.ShowEvents', {
    requires: ['Ext.mixin.Observable'],

    onClassMixedIn: function (cls) {
        cls.prototype.fireEvent = function () {
            console.log.apply(console, arguments);
            Ext.mixin.Observable.prototype.fireEvent.apply(this, arguments);
        };
    }
});
~~~~~

Вариант для Ext JS:

~~~~~javascript
Ext.define('Ext.debug.ShowEvents', {
    requires: ['Ext.util.Observable'],

    onClassMixedIn: function (cls) {
        cls.prototype.fireEvent = function () {
            console.log.apply(console, arguments);
            Ext.util.Observable.prototype.fireEvent.apply(this, arguments);
        };
    }
});
~~~~~

Не забудьте указать фреймворку, где этот файл лежит. Использовать его нужно приблизительно так:

~~~~~javascript
Ext.define('App.view.MyComponent', {
    extend: 'Ext.Component', 
    mixins: ['Ext.debug.ShowEvents'],

    ...
});
~~~~~~

Вместо `Ext.Component` подойдет любой класс с `Ext.mixin.Observable` (`Ext.util.Observable` для Ext JS). Теперь в консоли вы увидите все события, которые генерирует этот класс. 

Если что-то непонятно, можно спросить в комментариях.