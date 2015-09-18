// module Test.App

var MockApp = require('../../testJs/MockApp');
var MockRequest = require('../../testJs/MockRequest');

exports.createMockApp = function() {
    var properties = {
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
    return new MockApp(properties);
}

exports.createMockMiddleware = function(mockApp) {
    return function(request, response, next) {
        response.set("X-Mock-Middleware", request.get("X-Test-Value-To-Return"));
    }
}

exports.createMockRequest = function(method) {
    return function(url) {
        return new MockRequest(method, url);
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
