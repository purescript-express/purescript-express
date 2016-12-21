
var Url = require('url');

// String -> String -> MockRequest
var MockRequest = function(method, url) {
    this.method = method;
    this.url = url;
    this.originalUrl = url;
    this.headers = {};
    this.body = {};
    this.cookies = {};
    this.signedCookies = {};
    this.params = {};

    // NOTE: to be initialized when corresponding handler is called
    this.route = {path: null};

    this.initHardcodedValues();
    this.parseUrlAndInit();
}

// String -> String -> MockRequest
MockRequest.prototype.setHeader = function(headerName) {
    var self = this;
    return function(headerValue) {
        self.headers[headerName] = headerValue;
        return self;
    };
}

// String -> MockRequest
MockRequest.prototype.setBody = function(value) {
    var self = this;
    self.body = value;
    return self;
}

// String -> String -> MockRequest
MockRequest.prototype.setBodyParam = function(paramName) {
    var self = this;
    return function(paramValue) {
        self.body[paramName] = paramValue;
        return self;
    };
}

// String -> String -> MockRequest
MockRequest.prototype.setRouteParam = function(paramName) {
    var self = this;
    return function(paramValue) {
        self.params[paramName] = paramValue;
        return self;
    };
}

// String -> String -> MockRequest
MockRequest.prototype.setCookie = function(cookieName) {
    var self = this;
    return function(cookieValue) {
        self.cookies[cookieName] = cookieValue;
        return self;
    };
}

// String -> String -> MockRequest
MockRequest.prototype.setSignedCookie = function(cookieName) {
    var self = this;
    return function(cookieValue) {
        self.signedCookies[cookieName] = cookieValue;
        return self;
    };
}

MockRequest.prototype.setRoute = function(routePath) {
    this.route.path = routePath;
}

MockRequest.prototype.initHardcodedValues = function() {
    this.fresh = true;
    this.stale = false;
    this.xhr = false;
    this.ip = "0.0.0.0";
    this.ips = ["0.0.0.0", "0.0.0.1", "0.0.0.2"];
}

MockRequest.prototype.parseUrlAndInit = function() {
    var parsedUrl = Url.parse(this.url, true);
    this.path = parsedUrl.pathname;
    this.protocol = parsedUrl.protocol.slice(0, -1);
    this.hostname = parsedUrl.hostname;
    this.query = parsedUrl.query;
    this.subdomains = this.hostname.split(".").slice(0, -2);
    this.secure = this.protocol == "https";
}

MockRequest.prototype.checkAccepts = function(header, value) {
    var values = (this.headers[header] || "").split(",")
        .map(function(str, i) { return str.trim(); });
    if (values.indexOf(value) >= 0) {
        return value;
    }
}

/* Methods mocking corresponding Express.js methods */

MockRequest.prototype.get = function(headerName) {
    return this.headers[headerName];
}

MockRequest.prototype.accepts = function(type) {
    return this.checkAccepts('Accept', type);
}

MockRequest.prototype.acceptsCharset = function(type) {
    return this.checkAccepts('Accept-Charset', type);
}

MockRequest.prototype.acceptsLanguage = function(type) {
    return this.checkAccepts('Accept-Language', type);
}

MockRequest.prototype.is = function(type) {
    return this.headers['Content-Type'] == type;
}

module.exports = MockRequest;
