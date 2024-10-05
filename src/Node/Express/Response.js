export const _cwd = () => process.cwd()
export const _sendFileExt = (resp, path, opts, cb) => resp.sendFile(path, opts, cb);
export const _setStatus = (resp, code) => resp.status(code);
export const _setContentType = (resp, t) => resp.type(t);
export const _getHeader = (resp, field) => resp.get(field);
export const _setHeader = (resp, field, val) => resp.set(field, val);
export const _setCookie = (resp, name, value, opts) => resp.cookie(name, value, opts);
export const _clearCookie = (resp, name, path) => resp.clearCookie(name, { path: path || '/' });
export const _send = (resp, data) => resp.send(data);
export const _end = (resp) => resp.end();
export const _sendJson = (resp, data) => resp.json(data);
export const _sendJsonp = (resp, data) => resp.jsonp(data);
export const _redirectWithStatus = (resp, status, url) => resp.redirect(status, url);
export const _setLocation = (resp, url) => resp.location(url); // url or 'back'
export const _setAttachment = (resp, filename) => resp.attachment(filename);
export const _downloadExt = (resp, path, cb) => resp.download(path, cb)
export const _downloadExtWithName = (resp, path, name, cb) => resp.download(path, name, cb);
export const _headersSent = (resp) => resp.headersSent;
export const _render = (res, view, data) => res.render(view, data);
