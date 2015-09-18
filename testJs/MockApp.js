
var MockRequest = require('./MockRequest');
var MockResponse = require('./MockResponse');

var Handler = function(method, route, useOnError, fn) {
    this.method = method;
    this.route = route;
    this.useOnError = useOnError;
    if (useOnError) {
        this.run = function(app, error, req, resp, next) {
            return fn.apply(app, [error, req, resp, next]);
        };
    } else {
        this.run = function(app, req, resp, next) {
            return fn.apply(app, [req, resp, next]);
        };
    }
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
        args.unshift(method)
        return this.httpMethod.apply(this, args);
    }
}

MockApp.prototype.set = function(propertyName, value) {
    this.properties[propertyName] = value;
}

MockApp.prototype.use = function() {
    var use = function(handler) {
        if (handler.length == 3) {
            this.handlers.push(new Handler(null, null, false, handler));
        } else if (handler.length == 4) {
            this.handlers.push(new Handler(null, null, true, handler));
        }
    }

    var useAtRoute = function(route, handler) {
        this.handlers.push(new Handler(null, route, false, handler));
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
    this.handlers.push(new Handler(method, route, false, handler));
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

MockApp.prototype.emulate = function(request, error) {
    var isError = error != null;
    var handlerMatches = function(handler) {
        if (isError) {
            return handler.useOnError;
        } else {
            var methodMatched = (handler.method == null || handler.method == request.method);
            var routeMatched = (handler.route == null || request.path.startsWith(handler.route));
            return methodMatched && routeMatched && !handler.useOnError;
        }
    };

    var response = new MockResponse();
    var app = this;
    this.handlers.forEach(function (handler, i) {
        if (handlerMatches(handler)) {
            if (isError) {
                handler.run(app, error, request, response, function() {});
            } else {
                handler.run(app, request, response, function() {});
            }
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

module.exports = MockApp;
