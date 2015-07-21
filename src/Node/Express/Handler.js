// module Node.Express.Handler

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
