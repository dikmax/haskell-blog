---
title: Именование объектов в JavaScript
date: 2012-08-13T11:44:41+03:00
tags: closure compiler, inheritedjs, javascript, программирование
---

Вы же помните предыдущий пост про [библиотеку для создания объектов](http://dikmax.name/post/jsclass) на JavaScript? Так вот, я начал разбираться, откуда браузер может брать имена для классов. Как и ожидалось, разработчики браузеров даже и не пытались создать какой-нибудь общий стандарт, и каждый работает так, как ему хочется. Мне удалось обнаружить 2 способа задания имени класса (или 3, смотря как считать). Я, конечно же, могу заблуждаться, и вполне себе возможно существование каких-нибудь других недокументированных вариантов задания имени класса. В комментариях можно мне про это сказать.

Лучше всего дела обстоят у Chrome. Кто бы мог подумать? Он берет имя из функции конструктора вне зависимости от вида записи. 

~~~~~javascript
some.namespace.ClassA = function () {};

console.log(new some.namespace.ClassA()); // some.namespace.ClassA

function ClassB () {}

console.log(new ClassB()); // ClassB

var ClassC = function ClassD () {}

console.log(new ClassC()); // ClassD
console.log(new ClassD()); // error: undefined
~~~~~

Вслед за ним идет Firefox с Firebug. Он понимает только указание имени после слова function. 

~~~~~javascript
some.namespace.ClassA = function () {};

console.log(new some.namespace.ClassA()); // Object

function ClassB () {}

console.log(new ClassB()); // ClassB

var ClassC = function ClassD () {}

console.log(new ClassC()); // ClassD
console.log(new ClassD()); // error: undefined
~~~~~

С IE и Opera дела обстоят одинаково. Они подписывают только названия встроенных объектов. 

~~~~~javascript
some.namespace.ClassA = function () {};

console.log(new some.namespace.ClassA()); // Object

function ClassB () {}

console.log(new ClassB()); // Object

var ClassC = function ClassD () {}

console.log(new ClassC()); // Object
console.log(new ClassD()); // error: undefined
~~~~~

Все это наводит на мысль, что простым и очевидным способом создавать объекты не получится. Сначала нам понадобится определить браузеров, так как каких-то конкретных доступных фич в данном случае нет. А поскольку единственная возможность задать имя класса --- напрямую его указать, то eval будет нашим лучшим другом. И уже из этого последует тонкая настройка Google Closure Compiler, чтобы он не трогал важные для выполнения скрипта переменные. 
Поделюсь секретом: создание классов уже работает, и в скором времени, после еще некоторых правок, я выложу версию 0.1 библиотеки на Github. 