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