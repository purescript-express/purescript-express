
var MockRequest = require('./MockRequest');
var MockResponse = require('./MockResponse');
var Handler = require('./Handler');

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

module.exports = MockApp;
