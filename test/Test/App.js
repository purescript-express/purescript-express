// module Test.App

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
            handler(method, route, "OK");
        },
        mockUse: function(handler) {
            if (handler.length == 3) {
                handler("request", "response", "next");
            } else if (handler.length == 4) {
                handler("error", "request", "response", "next");
            }
        },
        mockUseAtRoute: function(route, handler) {
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

exports.registerCall = function(mockApp, callData) {
    mockApp["mockCallData"] = callData;
}

exports.getCallData = function(mockApp) {
    return mockApp["mockCallData"] || "";
}
