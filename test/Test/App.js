// module Test.App

exports.createMockMiddleware = function(mockApp) {
    return function(request, response, next) {
        response.set("X-Test-Response-Header", request.get("X-Test-Request-Header"));
    }
}
