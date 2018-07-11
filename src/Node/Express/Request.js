// module Node.Express.Request

exports._getRouteParam = function (req, name, nothing, just) {
    return function () {
        if (req.params[name] != null) {
            return just(req.params[name]);
        }
        return nothing;
    };
};

exports._getRoute = function (req) {
    return function () {
        return req.route.path;
    };
};

exports._getBody = function (req) {
    return function () {
        return req.body;
    };
};

exports._getBodyParam = function (req, name, nothing, just) {
    return function () {
        if (req.body && name in req.body && req.body[name] != null) {
            return just(req.body[name]);
        }
        return nothing;
    };
};

exports._getQueryParam = function (req, name, nothing, just) {
    return function () {
        if (req.query && name in req.query && req.query[name] != null) {
            return just(req.query[name]);
        }
        return nothing;
    };
};

exports._getCookie = function (req, name, nothing, just) {
    return function () {
        if (req.cookies[name] != null) {
            return just(req.cookies[name]);
        }
        return nothing;
    }
};

exports._getSignedCookie = function (req, name, nothing, just) {
    return function () {
        if (req.signedCookies[name] != null) {
            return just(req.signedCookies[name]);
        }
        return nothing;
    };
};

exports._getHeader = function (req, field, nothing, just) {
    return function () {
        if (req.get(field) != null) {
            return just(req.get(field));
        }
        return nothing;
    };
};

exports._accepts = function (req, types, nothing, just) {
    return function () {
        if (req.accepts(types) != null) {
            return just(req.accepts(types));
        }
        return nothing;
    };
};

exports._acceptsCharset = function (req, charset, nothing, just) {
    return function () {
        if (req.acceptsCharset(charset) != null) {
            return just(req.acceptsCharset(charset));
        }
        return nothing;
    };
};

exports._acceptsLanguage = function (req, language, nothing, just) {
    return function () {
        if (req.acceptsLanguage(language) != null) {
            return just(req.acceptsLanguage(language));
        }
        return nothing;
    };
};

exports._hasType = function (req, type) {
    return function () {
        return req.is(type);
    };
};

exports._getRemoteIp = function (req) {
    return function () {
        return req.ip;
    };
};

exports._getRemoteIps = function (req) {
    return function () {
        return req.ips;
    };
};

exports._getPath = function (req) {
    return function () {
        return req.path;
    };
};

exports._getHostname = function (req) {
    return function () {
        return req.hostname;
    };
};

exports._getSubdomains = function (req) {
    return function () {
        return req.subdomains;
    };
};

exports._isFresh = function (req) {
    return function () {
        return req.fresh;
    };
};

exports._isStale = function (req) {
    return function () {
        return req.stale;
    };
};

exports._isXhr = function (req) {
    return function () {
        return req.xhr;
    };
};

exports._getProtocol = function (req) {
    return function () {
        return req.protocol;
    };
};

exports._getMethod = function (req) {
    return function () {
        return req.method;
    };
};

exports._getUrl = function (req) {
    return function () {
        return req.url;
    };
};

exports._getOriginalUrl = function (req) {
    return function () {
        return req.originalUrl;
    };
};

exports._getData = function (req, field, nothing, just) {
    return function () {
        if (req.userData && req.userData[field] != null) {
            return just(req.userData[field]);
        }
        return nothing;
    };
};

exports._setData = function (req, field, val) {
    return function () {
        req.userData = req.userData || {};
        req.userData[field] = val;
    };
};

