// module Node.Express.App

exports.mkApplication = function() {
    var express = require('express');
    return express();
}

exports._httpServer = function(app) {
    return function() {
        var http = require('http');
        var server = http.createServer(app);
        return server;
    }
}

exports._httpsServer = function(app) {
    return function() {
        var https = require('https');
        var server = https.createServer(app);
        return server;
    }
}

exports._listenHttp = function(app) {
    return function(port) {
        return function(cb) {
            return function() {
                var http = require('http');
                var server = http.createServer(app);
                server.listen(port, function(e) {
                    return cb(e)();
                });
                return server;
            }
        }
    }
}

exports._listenHttps = function(app) {
    return function(port) {
        return function(opts) {
            return function(cb) {
                return function() {
                    var https = require('https');
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

exports._listenPipe = exports._listenHttp;

exports._listenHostHttp = function(app) {
    return function(port) {
        return function(host) {
            return function(cb) {
                return function() {
                    var http = require('http');
                    var server = http.createServer(app);
                    server.listen(port, host, function(e) {
                        return cb(e)();
                    });
                    return server;
                }
            }
        }
    }
}

exports._listenHostHttps = function(app) {
    return function(port) {
        return function(host) {
            return function(opts) {
                return function(cb) {
                    return function() {
                        var https = require('https');
                        var server = https.createServer(opts, app);
                        server.listen(port, host, function(e) {
                            return cb(e)();
                        });
                        return server;
                    }
                }
            }
        }
    }
}

exports._http = function (app, method, route, handler) {
    return function () {
        app[method](route, function(req, resp, next) {
            return handler(req)(resp)(next)();
        });
    };
};

exports._use = function (app, mw) {
    return function () {
        app.use(function(req, resp, next) {
            return mw(req)(resp)(next)();
        });
    };
};

exports._useAt = function (app, route, mw) {
    return function () {
        app.use(route, function(req, resp, next) {
            return mw(req)(resp)(next)();
        });
    };
};

exports._useOnParam = function (app, name, cb) {
    return function () {
        app.param(name, function(req, resp, next, val) {
            return cb(val)(req)(resp)(next)();
        });
    };
};

exports._useOnError = function (app, cb) {
    return function () {
        app.use(function(err, req, resp, next) {
            return cb(err)(req)(resp)(next)();
        });
    };
};

exports._getProp = function (app, name, nothing, just) {
    return function () {
        if (app.get(name) != null) {
            return just(app.get(name));
        }
        return nothing;
    };
};

exports._setProp = function (app, name, val) {
    return function () {
        app.set(name, val);
    };
};

exports._useExternal = function (app, mw) {
    return function () {
        app.use(mw);
    };
};

exports._useAtExternal = function (app, route, mw) {
    return function () {
        app.use(route, mw);
    };
};
