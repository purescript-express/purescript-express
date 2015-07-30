// module Test.App

exports.putCall = function(mockApp, call) {
    mockApp["mockFnCalls"].push(call);
}

exports.getCalls = function(mockApp) {
    return mockApp["mockFnCalls"];
}

exports.clearCalls = function(mockApp) {
    mockApp["mockFnCalls"] = [];
}

function Call(name, args) {
    this.name = name;
    this["arguments"] = args;
}

exports.createMockApp = function() {

    var properties = {
        stringProperty: "string",
        intProperty: 42,
        floatProperty: 100.1,
        booleanProperty: true,
        booleanFalseProperty: false,
        arrayProperty: ["a", "b", "c"],
        emptyArrayProperty: []
    };

    var handlerArguments = ["request", "response", "next"];
    var errorHandlerArguments = ["error", "request", "response", "next"];

    var app = {
        set: function(propertyName, value) {
            properties[propertyName] = value;
        },
        use: function() {
            if (arguments.length == 1) {
                this.mockUse.apply(this, arguments);
            } else if (arguments.length == 2) {
                this.mockUseAtRoute.apply(this, arguments);
            }
        },
        mockBindHttp: function(method, route, handler) {
            exports.putCall(this, new Call(method, [route]));
            handler.apply(this, handlerArguments);
        },
        mockUse: function(handler) {
            exports.putCall(this, new Call("use", []));
            if (handler.length == 3) {
                handler.apply(this, handlerArguments);
            } else if (handler.length == 4) {
                handler.apply(this, errorHandlerArguments);
            }
        },
        mockUseAtRoute: function(route, handler) {
            exports.putCall(this, new Call("use", [route]));
            handler.apply(this, handlerArguments);
        }
    };

    var methods = [
        "all", "get", "post", "put", "delete",
        "options", "head", "trace"
    ];
    methods.forEach(function(method) {
        app[method] = function() {
            if (method == "get") {
                if (arguments.length == 1) {
                    var propertyName = arguments[0];
                    return properties[propertyName];
                }
            }

            var args = Array.prototype.slice.call(arguments);
            args.unshift(method)
            this.mockBindHttp.apply(this, args);
        };
    });

    return app;
}

exports.createMockMiddleware = function(mockApp) {
    return function(req, resp, next) {
        exports.putCall(mockApp, new Call("handler", [req, resp, next]));
    }
}
