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
                var server = http.createServer(app);
                server.listen(port, function(e) {
                    return cb(e)();
                });
                return server;
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
                    var server = https.createServer(opts, app);
                    server.listen(port, function(e) {
                        return cb(e)();
                    });
                    return server;
                }
            }
        }
    }
}
