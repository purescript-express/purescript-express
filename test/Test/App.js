// module Test.App

exports.mockMiddleware = function(testValue) {
    return function(request, response, next) {
        response.set("X-Test-Response-Header", testValue);
    };
}
