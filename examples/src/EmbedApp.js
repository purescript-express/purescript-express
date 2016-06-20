"use strict";

exports.realMain = function(attachFn) {
    return function() {
        var app = require('express')();
        app.get('/', function(req, res) {
            res.send('Hello!');
        })
        attachFn(app)();
        app.listen(8080);
    }
}
