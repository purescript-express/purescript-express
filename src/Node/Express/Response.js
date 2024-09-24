export function _cwd() {
    return process.cwd();
};

export function _sendFileExt(resp, path, opts, cb) {
    return function () {
        resp.sendFile(path, opts, function (err) {
            return cb(err)();
        });
    };
};

export function _setStatus(resp, code) {
    return function () {
        resp.status(code);
    };
};

export function _setContentType(resp, t) {
    return function () {
        resp.type(t);
    };
};

export function _getHeader(resp, field, nothing, just) {
    return function () {
        if (resp.get(field) != null) {
            return just(resp.get(field));
        }
        return nothing;
    };
};

export function _setHeader(resp, field, val) {
    return function () {
        resp.set(field, val);
    };
};

export function _setCookie(resp, name, value, opts) {
    return function () {
        resp.cookie(name, value, opts);
    };
};

export function _clearCookie(resp, name, path) {
    return function () {
        resp.clearCookie(name, { path: path || '/' });
    };
};

export function _send(resp, data) {
    return function () {
        resp.send(data);
    };
};

export function _end(resp) {
    return function () {
        resp.end();
    };
};


export function _sendJson(resp, data) {
    return function () {
        resp.json(data);
    };
};

export function _sendJsonp(resp, data) {
    return function () {
        resp.jsonp(data);
    };
};

export function _redirectWithStatus(resp, status, url) {
    return function () {
        resp.redirect(status, url);
    };
};

export function _setLocation(resp, url) {
    return function () {
        resp.location(url);
    };
};

export function _setAttachment(resp, filename) {
    return function () {
        resp.attachment(filename);
    };
};

export function _downloadExt(resp, path, name, cb) {
    return function () {
        if (name === "") {
            resp.download(path, function (err) { return cb(err)(); });
        } else {
            resp.download(path, name, function (err) { return cb(err)(); });
        }
    };
};

export function _headersSent(resp) {
    return function () {
        return resp.headersSent;
    };
};

export function _render(res, view, data) {
    return function () {
        return res.render(view, data);
    };
};
