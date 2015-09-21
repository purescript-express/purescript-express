// module Test.App

exports.mockMiddleware = function(request, response, next) {
    response.set("X-Test-Response-Header", request.get("X-Test-Request-Header"));
}
