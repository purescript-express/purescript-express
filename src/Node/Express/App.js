import express from 'express';
import http from 'http';
import https from 'https';

export function mkApplication() {
    return express();
}

export function _httpServer(app) {
    return function () {
        var server = http.createServer(app);
        return server;
    }
}

export function _httpsServer(app) {
    return function () {
        var server = https.createServer(app);
        return server;
    }
}

export function _listenHttp(app) {
    return function (port) {
        return function (cb) {
            return function () {
                var server = http.createServer(app);
                server.listen(port, function (e) {
                    return cb(e)();
                });
                return server;
            }
        }
    }
}

export function _listenHttps(app) {
    return function (port) {
        return function (opts) {
            return function (cb) {
                return function () {
                    var server = https.createServer(opts, app);
                    server.listen(port, function (e) {
                        return cb(e)();
                    });
                    return server;
                }
            }
        }
    }
}

export const _listenPipe = _listenHttp;

export function _listenHostHttp(app) {
    return function (port) {
        return function (host) {
            return function (cb) {
                return function () {
                    var server = http.createServer(app);
                    server.listen(port, host, function (e) {
                        return cb(e)();
                    });
                    return server;
                }
            }
        }
    }
}

export function _listenHostHttps(app) {
    return function (port) {
        return function (host) {
            return function (opts) {
                return function (cb) {
                    return function () {
                        var server = https.createServer(opts, app);
                        server.listen(port, host, function (e) {
                            return cb(e)();
                        });
                        return server;
                    }
                }
            }
        }
    }
}

export function _http(app, method, route, handler) {
    return function () {
        app[method](route, function (req, resp, next) {
            return handler(req)(resp)(next)();
        });
    };
};

export function _use(app, mw) {
    return function () {
        app.use(function (req, resp, next) {
            return mw(req)(resp)(next)();
        });
    };
};

export function _useAt(app, route, mw) {
    return function () {
        app.use(route, function (req, resp, next) {
            return mw(req)(resp)(next)();
        });
    };
};

export function _useOnParam(app, name, cb) {
    return function () {
        app.param(name, function (req, resp, next, val) {
            return cb(val)(req)(resp)(next)();
        });
    };
};

export function _useOnError(app, cb) {
    return function () {
        app.use(function (err, req, resp, next) {
            return cb(err)(req)(resp)(next)();
        });
    };
};

export function _getProp(app, name, nothing, just) {
    return function () {
        if (app.get(name) != null) {
            return just(app.get(name));
        }
        return nothing;
    };
};

export function _setProp(app, name, val) {
    return function () {
        app.set(name, val);
    };
};

export function _useExternal(app, mw) {
    return function () {
        app.use(mw);
    };
};

export function _useAtExternal(app, route, mw) {
    return function () {
        app.use(route, mw);
    };
};
