// module Node.Express.Response

exports._cwd = function () {
    return process.cwd();
};

exports._sendFileExt = function (resp, path, opts, cb) {
    return function () {
        resp.sendFile(path, opts, function(err) {
            return cb(err)();
        });
    };
};

exports._setStatus = function (resp, code) {
    return function () {
        resp.status(code);
    };
};

exports._setContentType = function (resp, t) {
    return function () {
        resp.type(t);
    };
};

exports._getHeader = function (resp, field, nothing, just) {
    return function () {
        if (resp.get(field) != null) {
            return just(resp.get(field));
        }
        return nothing;
    };
};

exports._setHeader = function (resp, field, val) {
    return function () {
        resp.set(field, val);
    };
};

exports._setCookie = function (resp, name, value, opts) {
    return function () {
        resp.cookie(name, value, opts);
    };
};

exports._clearCookie = function (resp, name, path) {
    return function () {
        resp.clearCookie(name, {path: path || '/'});
    };
};

exports._send = function (resp, data) {
    return function () {
        resp.send(data);
    };
};

exports._end = function (resp) {
    return function () {
        resp.end();
    };
};


exports._sendJson = function (resp, data) {
    return function () {
        resp.json(data);
    };
};

exports._sendJsonp = function (resp, data) {
    return function () {
        resp.jsonp(data);
    };
};

exports._redirectWithStatus = function (resp, status, url) {
    return function () {
        resp.redirect(status, url);
    };
};

exports._setLocation = function (resp, url) {
    return function () {
        resp.location(url);
    };
};

exports._setAttachment = function (resp, filename) {
    return function () {
        resp.attachment(filename);
    };
};

exports._downloadExt = function (resp, path, name, cb) {
    return function () {
        if (name === "") {
            resp.download(path, function(err) { return cb(err)(); });
        } else {
            resp.download(path, name, function(err) { return cb(err)(); });
        }
    };
};

exports._headersSent = function (resp) {
    return function () {
        return resp.headersSent;
    };
};

exports._render = function (res, view, data) {
    return function () {
        return res.render(view, data);
    };
};
