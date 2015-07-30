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
        mockBindHttp: function(method, route, handler) {
            handler(method, route, "OK");
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
