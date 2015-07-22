// module Node.Express.Internal.App

exports.intlMkApplication = function() {
    var express = module.require('express');
    return express();
}

exports.intlAppListenHttp = function(app) {
    return function(port) {
        return function(cb) {
            return function() {
                var http = module.require('http');
                http.createServer(app).listen(port, function(e) {
                    return cb(e)();
                });
            }
        }
    }
}

exports.intlAppListenHttps = function(app) {
    return function(port) {
        return function(opts) {
            return function(cb) {
                return function() {
                    var https = module.require('https');
                    https.createServer(opts, app).listen(port, function(e) {
                        return cb(e)();
                    });
                }
            }
        }
    }
}

exports.intlAppHttp = function (app, method, route, handler) {
    return function () {
        app[method](route, function(req, resp, next) {
            return handler(req)(resp)(next)();
        });
    };
};

exports.intlAppUse = function (app, mw) {
    return function () {
        app.use(function(req, resp, next) {
            return mw(req)(resp)(next)();
        });
    };
};

exports.intlAppUseAt = function (app, route, mw) {
    return function () {
        app.use(route, function(req, resp, next) {
            return mw(req)(resp)(next)();
        });
    };
};

exports.intlAppUseOnParam = function (app, name, cb) {
    return function () {
        app.param(name, function(req, resp, next, val) {
            return cb(val)(req)(resp)(next)();
        });
    };
};

exports.intlAppUseOnError = function (app, cb) {
    return function () {
        app.use(function(err, req, resp, next) {
            return cb(err)(req)(resp)(next)();
        });
    };
};

