---
title: AngularJS и параметры контроллеров
date: 2013-03-30T21:53:51+03:00
tags: angularjs, javascript, программирование
---

Те люди, которые работают с [AngularJS](http://angularjs.org/), знают про директиву [ngController](http://docs.angularjs.org/api/ng.directive:ngController). В нее передается функция, описывающая поведение блока с директивой. И тут начинается магия! Если наша функция выглядит так: 

~~~~~javascript
SomeController = function ($scope) {...};
~~~~~

то она будет вызвана с одним параметром — scope, связанным с контроллером. А если, она выглядит, например, так:

~~~~~javascript
SomeController = function ($http, $location, $scope) {...};
~~~~~

то передаваемых параметров станет внезапно три: два системных сервиса ($http и $location) и тот же самый scope.

Так как же фреймворк определяет, какие параметры нужны для вызова контроллера? Эта магия называется `Function.toString()`. Метод `toString()` у функции возвращает ее текст, а дальше дело техники: `/^function\s*[^\(]*\(\s*([^\)]*)\)/m`. Получили имена параметров, разобрали, подставили. Вот такой нетривиальный подход.

Детали можно посмотреть в [исходниках](https://github.com/angular/angular.js/blob/master/src/auto/injector.js#L45).
