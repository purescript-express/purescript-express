var MockResponse = function() {
    this.status = 0;
    this.contentType = "";
    this.headers = {};
    this.data = "";
    this.headersSent = false;
}

MockResponse.prototype.status = function(statusCode) {
    this.status = statusCode;
}

MockResponse.prototype.type = function(contentType) {
    this.contentType = contentType;
}

MockResponse.prototype.get = function(headerName) {
    return this.headers[headerName];
}

MockResponse.prototype.set = function(headerName, value) {
    this.headers[headerName] = value;
}

MockResponse.prototype.cookie = function(name, value, options) {
    throw "NotImplemented";
}

MockResponse.prototype.clearCookie = function(name, options) {
    throw "NotImplemented";
}

MockResponse.prototype.send = function(data) {
    if (typeof data === 'string') {
        this.data += data;
    } else {
        this.json(data);
    }
    this.headersSent = true;
}

MockResponse.prototype.json = function(obj) {
   this.send(JSON.stringify(obj));
}

MockResponse.prototype.jsonp = MockResponse.prototype.json;

MockResponse.prototype.redirect = function(status, url) {
    this.status(status);
    this.location(url);
}

MockResponse.prototype.location = function(url) {
    this.set("Location", url);
}

MockResponse.prototype.attachemnt = function(filename) {
    throw "NotImplemented";
}

MockResponse.prototype.sendFile = function(path, options, callback) {
    throw "NotImplemented";
}

MockResponse.prototype.download = function() {
    throw "NotImplemented";
}

module.exports = MockResponse;
