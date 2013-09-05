---
title: Наследование в JavaScript: скандалы, интриги, расследования
date: 2012-10-09T22:13:13+03:00
tags: javascript, программирование
---

Наследование в JavaScript\ --- одна из самых сложных и запутанных тем. Бо́льшая часть программистов даже не пытается лезть в нее, часть поменьше кое-что слышала и даже пытается что-то делать. И лишь небольшая часть разбирается в этой теме.

# Итак, наследование.

~~~~~javascript
var ParentClass = function () {}; // ParentClass contructor
ParentClass.prototype.method1 = function () {};
var ChildClass = function () {}; // ChildClass constructor
ChildClass.prototype = ParentClass.prototype;
ChildClass.prototype.method2 = function () {};
~~~~~

Почему-то некоторые уверены, что просто скопировав прототип, мы получим правильное наследование. Так вот, пример выше неверен. Если мы скопируем прототип таким образом, то `ChildClass` будет не просто наследником, а родительским классом, но с другим конструктором. А значит `method2`, который мы добавили в 5й сточке, будет так же в прототипе у `ParentClass`. Это получается потому, что присваивание объектов происходит по ссылке, а значит `ChildClass.prototype` после 4й строчки будет ссылаться на тот же объект, что и `ParentClass.prototype`.

# Вариант, увиденный совсем недавно.

~~~~~javascript
var ParentClass = function () {}; // ParentClass contructor
ParentClass.prototype.method1 = function () {};
var ChildClass = function () {}; // ChildClass constructor
ChildClass.prototype = ParentClass.prototype;
var child = new ChildClass();
child.method2 = function () {};
~~~~~

Очень похож на первый. Только расширять предлагается не прототип, а уже экземпляр класса. Да, код будет работать, но это плохой вариант. В таком случае нам нужно расширять класс каждый раз, когда мы создаем экземпляр класса. 

--- А давайте поместим расширение класса в конструктор,\ --- скажете вы.\ --- Например, вот так:

~~~~~javascript
var ChildClass = function () { // ChildClass constructor
    this.method2 = function () {};
}; 
~~~~~

Хм. Да, так тоже можно. Но у нас получится по одному экземпляру метода `method2` на каждый экземпляр класса, а не один на всех, как в случае прототипа. Поэтому\ --- нет.

# Вариант 3.

~~~~~javascript
var ParentClass = function () {}; // ParentClass contructor
ParentClass.prototype.method1 = function () {};
var ChildClass = function () {}; // ChildClass constructor
for (var prop in ParentClass.prototype) {
    if (ParentClass.prototype.hasOwnProperty(prop)) {
        ChildClass.prototype[prop] = ParentClass.prototype[prop];
    }
}
ChildClass.prototype.method2 = function () {};
~~~~~

Ну, этот вариант по крайней мере работает. И что самое интересное, работает правильно. Но есть несколько вещей, которые не очень хороши. Если у нас в прототипе родительского класса 100500 полей и методов, то в цикле нам придется пробежаться по ним всем и скопировать в прототип дочернего класса. А если при этом у нас много дочерних классов, то получится вообще кошмар. В памяти расплодится множество одинаковых объектов, содержащих перечисление всех методов и полей.

И еще, если мы добавим в самом конце один метод в родительский класс:

~~~~~javascript
ParentClass.prototype.method3 = function () {};
~~~~~

то он попадет только в него, а не в оба, т.к. связь между классами была потеряна на этапе копирования.

# Правильный вариант.

~~~~~javascript
var ParentClass = function () {}; // ParentClass contructor
ParentClass.prototype.method1 = function () {};
var ChildClass = function () {}; // ChildClass constructor

var tempConstructor = function() {};
tempConstructor.prototype = ParentClass.prototype;
ChildClass.prototype = new tempConstructor();
ChildClass.prototype.constructor = ChildClass;

ChildClass.prototype.method2 = function () {};
~~~~~

Нам нужно скопировать прототип, а самый быстрый способ это сделать --- создать экземпляр класса. И даже лучше! В этом случае родительский прототип уйдет на уровень глубже, а прототип дочернего класса окажется пустым. Плюс, если мы добавим в родительский прототип еще метод после того, как инициализировали дочерний, то он тоже будет в дочернем классе.

Что же происходит в примере выше? Мы создаем временный класс, такой же, как родительский, но у него пустой конструктор. А потом инициализируем прототип дочернего класса этим временным классом. И последний этап: нужно вернуть на место свойство `constructor` из дочернего класса.

Все просто? Наверное так.

# И еще правильный вариант, но не всегда.

~~~~~javascript
var ParentClass = function () {}; // ParentClass contructor
ParentClass.prototype.method1 = function () {};
var ChildClass = function () {}; // ChildClass constructor

ChildClass.prototype.__proto__ = ParentClass.prototype;

ChildClass.prototype.method2 = function () {};
~~~~~

Этот вариант делает все то же, что и предыдущий, только без создания временного класса. Мы просто берем и записываем прототип родителя в иерархию. Возникает вопрос, а зачем же тогда выбирать более сложный вариант, если есть попроще? Надеюсь, никто не сомневается, что причина выбора --- Internet Explorer. Наш любимый IE не поддерживает свойство `__proto__`, а значит код не будет работать. Но если ваш код никогда не будет запускаться в IE (может, вы под node.js или Phonegap пишете), тогда этот вариант ваш.

# Итог.

JavaScript очень гибкий язык, и в нем есть множество способов достичь одинакового результата. Как напишете, так и будет работать. Я просто предложил самый логичный и быстрый способ. Если у вас есть еще варианты или просто вопросы --- милости прошу в комментарии.