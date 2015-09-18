
var parseurl = require('parseurl');

/**
 * @param method :: String
 * @param url :: String
 * @param headers :: StrMap String
 * @param body :: StrMap String
 * @param cookies :: StrMap String ??
 */
var MockRequest = function(method, url) {
    this.method = method;
    this.url = url;
    this.headers = {};
    this.body = {};
    this.cookies = {};

    this.baseUrl = url;
    this.fresh = true;
    this.stale = false;

    // TODO: implement
    this.params = null;
    this.route = null;
}

MockRequest.prototype.setHeader = function(headerName) {
    var self = this;
    return function(headerValue) {
        return function() {
            self.headers[headerName] = headerValue;
        };
    };
}

MockRequest.prototype.setBodyParam = function(paramName) {
    var self = this;
    return function(paramValue) {
        return function() {
            self.body[paramName] = paramValue;
        };
    };
}

/// ---

MockRequest.prototype.get = function(headerName) {
    return this.headers[headerName];
}

module.exports = MockRequest;
