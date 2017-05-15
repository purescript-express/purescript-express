// module Test.Mock

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

var MockResponse = function() {
    this.statusCode = 0;
    this.headers = {};
    this.data = "";
    this.cookies = {};
    this.headersSent = false;
}

MockResponse.prototype.status = function(statusCode) {
    this.statusCode = statusCode;
}

MockResponse.prototype.type = function(contentType) {
    this.headers['Content-Type'] = contentType;
}

MockResponse.prototype.get = function(headerName) {
    return this.headers[headerName];
}

MockResponse.prototype.set = function(headerName, value) {
    this.headers[headerName] = value;
}

MockResponse.prototype.cookie = function(name, value, options) {
    this.cookies[name] = {name: name, value: value, options: options};
}

MockResponse.prototype.clearCookie = function(name, options) {
    delete this.cookies[name];
}

MockResponse.prototype.send = function(data) {
    if (typeof data === 'string') {
        this.data += data;
    } else {
        this.json(data);
    }
    this.headersSent = true;
}

MockResponse.prototype.end = function() {
    this.statusCode = 200;
    this.headersSent = true;
}

MockResponse.prototype.json = function(obj) {
   this.send(JSON.stringify(obj));
}

MockResponse.prototype.render = function(view, obj) {
   this.send('Rendered ' + view + ' with data: ' + JSON.stringify(obj));
}

MockResponse.prototype.jsonp = MockResponse.prototype.json;

MockResponse.prototype.redirect = function(statusCode, url) {
    this.status(statusCode);
    this.location(url);
}

MockResponse.prototype.location = function(url) {
    this.set("Location", url);
}

MockResponse.prototype.sendFile = function(path, options, callback) {
    if (typeof options === 'object' && options.triggerError) {
        return callback(this);
    }

    this.set("X-Filepath", path);
    this.json(options);
}

MockResponse.prototype.download = function() {
    if (arguments.length == 2) {
        var args = Array.prototype.slice.call(arguments);
        args.unshift(arguments[0])
        this.downloadImpl.apply(this, args);
    } else if (arguments.length == 3) {
        this.downloadImpl.apply(this, arguments);
    }
}

MockResponse.prototype.downloadImpl = function(path, filename, callback) {
    if (filename == "triggerError") {
        return callback(this);
    }

    this.set("X-Filepath", filename);
    this.set("X-Real-Filepath", path);
}

var Handler = function(method, route, param, useOnError, fn) {
    this.method = method;
    this.route = route;
    this.param = param;
    this.useOnError = useOnError;
    if (useOnError) {
        this.run = function(app, error, req, resp, next) {
            return fn.apply(app, [error, req, resp, next]);
        };
    } else if (param) {
        this.run = function(app, req, resp, next) {
            req.setRoute(route);
            var paramValue = req.params[param];
            return fn.apply(app, [req, resp, next, paramValue]);
        };
    } else {
        this.run = function(app, req, resp, next) {
            req.setRoute(route);
            return fn.apply(app, [req, resp, next]);
        };
    }
}

Handler.prototype.matches = function(request) {
    return this.methodMatches(request)
        && this.routeMatches(request)
        && this.paramMatches(request)
        && !this.useOnError;
}

Handler.prototype.methodMatches = function(request) {
    return this.method == null || this.method == request.method;
}

Handler.prototype.routeMatches = function(request) {
    var routeRx = new RegExp(this.route);
    return this.route == null || routeRx.test(request.path);
}

Handler.prototype.paramMatches = function(request) {
    return this.param == null || typeof request.params[this.param] == 'string';
}

var MockApp = function(properties) {
    this.properties = properties;
    this.handlers = [];
}

MockApp.prototype.get = function() {
    if (arguments.length == 1) {
        var propertyName = arguments[0];
        return this.properties[propertyName];
    } else {
        var args = Array.prototype.slice.call(arguments);
        args.unshift("get")
        return this.httpMethod.apply(this, args);
    }
}

MockApp.prototype.set = function(propertyName, value) {
    this.properties[propertyName] = value;
}

MockApp.prototype.use = function() {
    var use = function(handler) {
        if (handler.length == 3) {
            this.handlers.push(new Handler(null, null, null, false, handler));
        } else if (handler.length == 4) {
            this.handlers.push(new Handler(null, null, null, true, handler));
        }
    }

    var useAtRoute = function(route, handler) {
        this.handlers.push(new Handler(null, route, null, false, handler));
    }

    if (arguments.length == 1) {
        use.apply(this, arguments);
    } else if (arguments.length == 2) {
        useAtRoute.apply(this, arguments);
    }
}

MockApp.prototype.httpMethod = function(method, route, handler) {
    if (method == "all") {
        method = null;
    }
    this.handlers.push(new Handler(method, route, null, false, handler));
}

var methods = [
    "all", "post", "put", "delete",
    "options", "head", "trace"
];
methods.forEach(function(method) {
    MockApp.prototype[method] = function() {
        var args = Array.prototype.slice.call(arguments);
        args.unshift(method)
        this.httpMethod.apply(this, args);
    };
});

MockApp.prototype.param = function(name, handler) {
    this.handlers.push(new Handler(null, null, name, false, handler));
}

MockApp.prototype.emulate = function(request, error) {
    var isError = error != null;
    var response = new MockResponse();
    var app = this;
    this.handlers.forEach(function (handler, i) {
        if (isError && handler.useOnError) {
            handler.run(app, error, request, response, function() {});
        } else if (handler.matches(request)) {
            handler.run(app, request, response, function() {});
        }
    });
    return response;
}

MockApp.prototype.sendRequest = function(request) {
    return this.emulate(request, null);
}

MockApp.prototype.sendError = function(request, error) {
    return this.emulate(request, error);
}

exports.createMockApp = function() {
    var predefinedProperties = {
        string: "string",
        emptyString: "",
        fortyTwo: 42,
        zeroInt: 0,
        hundredPointOne: 100.1,
        zeroFloat: 0.0,
        trueBoolean: true,
        falseBoolean: false,
        abcArray: ["a", "b", "c"],
        emptyArray: []
    };
    return new MockApp(predefinedProperties);
}

exports.createMockRequest = function(method) {
    return function(url) {
        return function() {
            return new MockRequest(method, url);
        };
    };
}

exports.sendMockRequest = function(mockApp) {
    return function(request) {
        return function() {
            return mockApp.sendRequest(request);
        };
    };
}

exports.sendMockError = function(mockApp) {
    return function(request) {
        return function(error) {
            return function() {
                return mockApp.sendError(request, new Error(error));
            };
        };
    };
}
