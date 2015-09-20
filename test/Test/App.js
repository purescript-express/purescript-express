// module Test.App

exports.createMockMiddleware = function(mockApp) {
    return function(request, response, next) {
        response.set("X-Mock-Middleware", request.get("X-Test-Value-To-Return"));
    }
}
