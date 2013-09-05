---
title: Inherited.js design solutions
date: 2012-08-16T14:18:48+03:00
tags: inheritedjs, javascript, вопрос, программирование
---

Итак, я написал некоторое количество тестов, поправил несколько багов. Пришла пора задуматься: что же делать дальше? Как должно выглядеть API будущей библиотеки? Пожалуй, я выложу несколько вариантов своего видения, чтобы вы могли высказать свои за и против. Очень надеюсь на ваши комментарии.

Вариант 1:

~~~~~javascript
NewClass = Class.extend("OptionalDisplayName", {
    constructor: function () {},

    properties: {
        property1: {
            set: function(value) {this.properties_.property1 = value;},
            get: function() {return this.properties_.property1;},
            initial: 'Initial value'
        },
        property2: { // Property with default getter and no setter
            get: true
        }
    },

    statics: {
        staticField1: null,
        staticMethod1: function(arg1) {return arg1;},

        properties: {
            staticProperty1: {get: true, set: true}
        }
    },

    field1: null,
    method1: function() {}
});

NewClass2 = NewClass.extend({
    constructor: function () { this.inherited(); },
    method1: function() { this.inherited(); },
    method2: function() {}
});
~~~~~

Вариант 2:

~~~~~javascript
NewClass = createClass({
    __name__: "OptionalDisplayName",
    constructor: function () {},

    property1: __property__({
        set: function(value) {this.properties_.property1 = value;},
        get: function() {return this.properties_.property1;},
        initial: 'Initial value'
    }),

    property2: __property__({ // Property with default getter and no setter
        get: true
    }),

    staticField1: __static__(null),
    staticMethod1: __static__(function(arg1) {return arg1;}),

    staticProperty1: __staticProperty__({get: true, set: true}),

    field1: null,
    method1: function() {}
});

NewClass2 = createClass({
    constructor: function () { this.inherited(); },
    extend: NewClass,

    method1: function() { this.inherited(); },
    method2: function() {}
});
~~~~~

Какой из вариантов кажется более логичным и удобным? Может, составить какой-то смешанный вариант или у вас есть предложения, кардинально отличающиеся от этих двух? Не стесняемся, пишем комментарии.