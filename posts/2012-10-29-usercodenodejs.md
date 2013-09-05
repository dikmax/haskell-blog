---
title: Выполнение пользовательского кода в nodejs
date: 2012-10-29T15:47:08+03:00
tags: javascript, nodejs, безопасность, программирование, работа
---

Мне тут по долгу службы пришлось столкнуться с [node.js](http://nodejs.org/). Это такой серверный JavaScript, если что. И в его основе лежит [V8](http://code.google.com/p/v8/) - JS-движок от Google, который стоит в Google Chrome.

Весьма вероятно, что в ближайшее время вы увидите еще много постов про node.js в этом блоге, а пока вот вам практическая задача.

У нас есть сервис с некоторым набором данных, неважно каких. Этот сервис позволяет выполнять пользовательские операции над этими данными (например, валидацию). Т.е. любой пользователь может зайти, написать что-то вроде `setValid(data.a == 1)` --- и код будет выполнен. Понятное дело, что сразу возникает вопрос безопасности. Ведь никто не мешает пользователю подключить библиотеку работы с файловой системой и сделать что-нибудь нехорошее:

~~~~~javascript
var fs = require('fs');
fs.readdir('~', function(err, files) {
  files.forEach(function (file) {
    fs.unlink('~/' + file);
  });
});
~~~~~

Или может вообще не возвращать результат, а загрузить процессор на 100%. Мы ведь ничего не можем гарантировать, когда дело доходит по пользовательского кода. Да ведь там могут быть и просто синтаксические ошибки.

Итак, мое решение.

Во-первых, весь пользовательский код выполняется в отдельном процессе. Этот процесс принудительно завершается по истечении некоторого времени (секунд 10 или около того).

~~~~~javascript
// parent.js
var child = child_process.fork('./child.js');
child.on('message', function(message) {
    // message будет содержать ответ от потомка
});
setTimeout(function () {
    child.kill();
    console.log('Task was killed by timeout');
}, 10000);
child.send(data); // Оправляем параметры для выполнения
~~~~~

~~~~~javascript
// child.js
process.on('message', function (message) {
    // message будет содержать параметры выполнения
    // TODO ????
    process.send(result); // Отправляем результат
});
~~~~~

Во-вторых, я оборачиваю вызов `eval` в `try..catch`, хотя да, это очевидно.

В-третьих, `eval` вызывается в специальной функции с кучей неиспользуемых параметров. Имена этих параметров соответствуют всем доступным глобальным именам и они будут равны `undefined` в момент работы `eval`.

~~~~~javascript
var libs = { // Разрешенные библиотеки
    "libxmljs": require("libxmljs") 
};
var l = "";
for (var i in libs) {
    if (libs.hasOwnProperty(i)) {
        if (l) {
            l += ',';
        }
        l += i + '=this.libs.' + i;
    }
}
l = "var " + l + ';';

// Класс ответственный за выполнение кода
var RunnerClass = function (code) {
    this.code = code;
};
RunnerClass.prototype.__proto__ = EventEmitter.prototype;

RunnerClass.prototype.run = function (data) {
    var code = this.code;

    // Объект, который представляет собой контекст this для выполняемого кода. Все, что описано тут, доступно во вложенном коде.
    var context = new EventEmitter();
    context["data"] = data;
    context["libs"] = libs;
    context["finish"] = function () {
        this.emit("finish");
    };
    context.on("finish", function () {
        this.emit("finish", context.result);
        console.log('Result', context.result);        
    }.bind(this));

    // Вот эти параметры и прячут глобальные переменные
    var sandbox = function(Buffer, CodeRunner, EventsEmitter, context, global, exports, i, libs, process, module, require,  __dirname, __filename) {
        try {
            eval(l + '!function(code,l){' + code + '}.call(this)'); // Дополнительно прячем в именах параметров код и подключение библиотек.
        } catch (e) {
            console.log("Code execution error: " + e.message);
        }
    };
    // Вызываем sandbox без параметров, так что все имена становятся равными undefined
    sandbox.call(context); 
};
~~~~~

Понятно или еще комментариев дописать?

Нужно только осторожно следить за тем, какие функции и классы открывать для пользователей. Возможно, я что-то не предусмотрел, поправьте меня в таком случае.
