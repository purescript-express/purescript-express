export function _getRouteParam(req, name, nothing, just) {
    return function () {
        if (req.params[name] != null) {
            return just(req.params[name]);
        }
        return nothing;
    };
};

export function _getRouteParams(req) {
    return function () {
        return req.params;
    };
};

export function _getRoute(req) {
    return function () {
        return req.route.path;
    };
};

export function _getBody(req) {
    return function () {
        return req.body;
    };
};

export function _getBodyParam(req, name, nothing, just) {
    return function () {
        if (req.body && name in req.body && req.body[name] != null) {
            return just(req.body[name]);
        }
        return nothing;
    };
};

export function _getQueryParam(req, name, nothing, just) {
    return function () {
        if (req.query && name in req.query && req.query[name] != null) {
            return just(req.query[name]);
        }
        return nothing;
    };
};

export function _getCookie(req, name, nothing, just) {
    return function () {
        if (req.cookies[name] != null) {
            return just(req.cookies[name]);
        }
        return nothing;
    }
};

export function _getSignedCookie(req, name, nothing, just) {
    return function () {
        if (req.signedCookies[name] != null) {
            return just(req.signedCookies[name]);
        }
        return nothing;
    };
};

export function _getHeader(req, field, nothing, just) {
    return function () {
        if (req.get(field) != null) {
            return just(req.get(field));
        }
        return nothing;
    };
};

export function _getHeaders(req) {
    return function () {
        return req.headers;
    };
};

export function _accepts(req, types, nothing, just) {
    return function () {
        if (req.accepts(types) != null) {
            return just(req.accepts(types));
        }
        return nothing;
    };
};

export function _acceptsCharset(req, charset, nothing, just) {
    return function () {
        if (req.acceptsCharset(charset) != null) {
            return just(req.acceptsCharset(charset));
        }
        return nothing;
    };
};

export function _acceptsLanguage(req, language, nothing, just) {
    return function () {
        if (req.acceptsLanguage(language) != null) {
            return just(req.acceptsLanguage(language));
        }
        return nothing;
    };
};

export function _hasType(req, type) {
    return function () {
        return req.is(type);
    };
};

export function _getRemoteIp(req) {
    return function () {
        return req.ip;
    };
};

export function _getRemoteIps(req) {
    return function () {
        return req.ips;
    };
};

export function _getPath(req) {
    return function () {
        return req.path;
    };
};

export function _getHostname(req) {
    return function () {
        return req.hostname;
    };
};

export function _getSubdomains(req) {
    return function () {
        return req.subdomains;
    };
};

export function _isFresh(req) {
    return function () {
        return req.fresh;
    };
};

export function _isStale(req) {
    return function () {
        return req.stale;
    };
};

export function _isXhr(req) {
    return function () {
        return req.xhr;
    };
};

export function _getProtocol(req) {
    return function () {
        return req.protocol;
    };
};

export function _getMethod(req) {
    return function () {
        return req.method;
    };
};

export function _getUrl(req) {
    return function () {
        return req.url;
    };
};

export function _getOriginalUrl(req) {
    return function () {
        return req.originalUrl;
    };
};

export function _getData(req, field, nothing, just) {
    return function () {
        if (req.userData && req.userData[field] != null) {
            return just(req.userData[field]);
        }
        return nothing;
    };
};

export function _setData(req, field, val) {
    return function () {
        req.userData = req.userData || {};
        req.userData[field] = val;
    };
};
