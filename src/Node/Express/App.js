import express from 'express';
import http from 'http';
import https from 'https';

export const mkApplication = () => express();
export const _getProp = (app, name) => app.get(name);
export const _setProp = (app, name, val) => app.set(name, val);
export const _http = (app, method, route, handler) => { app[method](route, handler) }; // method is get, post, etc
export const _httpServer = (app) => http.createServer(app);
export const _httpsServer = (app) => https.createServer(app);
export const _listenHttp = (app, port, cb) => (http.createServer(app).listen(port, cb));
export const _listenHttps = (app, port, opts, cb) => (https.createServer(opts, app).listen(port, cb));
export const _listenPipe = _listenHttp;
export const _listenHostHttp = (app, port, host, cb) => (http.createServer(app).listen(port, host, cb));
export const _listenHostHttps = (app, port, host, opts, cb) => (https.createServer(opts, app).listen(port, host, cb));

export const _use = (app, mw) => app.use(mw); // req, res, next OR err, req, res, next
export const _useAt = (app, route, mw) => app.use(route, mw); // req, res, next OR err, req, res, next

export const _param = (app, name, cb) => app.param(name, cb); // req, res, next, id https://expressjs.com/en/5x/api.html#app.param
