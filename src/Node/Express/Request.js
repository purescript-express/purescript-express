// module Node.Express.Request

exports._getRouteParam = function (req, name) {
    return function () {
        return req.params[name];
    };
};

exports._getRoute = function (req) {
    return function () {
        return req.route.path;
    };
};

exports._getBodyParam = function (req, name) {
    return function () {
        return req.body == null ? void 0 : req.body[name];
    };
};

exports._getQueryParams = function (req) {
    return function () {
        return req.url.split('?')[1] || '';
    };
};

exports._getCookie = function (req, name) {
    return function () {
        return req.cookies[name];
    }
};

exports._getSignedCookie = function (req, name) {
    return function () {
        return req.signedCookies[name];
    };
};

exports._getHeader = function (req, field) {
    return function () {
        return req.get(field);
    };
};

exports._accepts = function (req, types) {
    return function () {
        return req.accepts(types);
    };
};

exports._acceptsCharset = function (req, charset) {
    return function () {
        return req.acceptsCharset(charset);
    };
};

exports._acceptsLanguage = function (req, language) {
    return function () {
        return req.acceptsLanguage(language);
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

