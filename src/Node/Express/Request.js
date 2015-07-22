// module Node.Express.Request

exports.reqRoute = function (req) {
    return function () {
        return req.route.path;
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
