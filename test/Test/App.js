// module Test.App

exports.mockMiddleware = function(testValue) {
    return function(request, response, next) {
        response.set("X-Test-Response-Header", testValue);
    };
}

exports.mockThirdMain = function(testValue) {
    return function(app) {
        return function() {
            app.get('/', function(request, response) {
                response.set("X-Test-Response-Header", testValue);
            });
            return {};
        };
    };
}
