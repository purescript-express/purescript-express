export function mockMiddleware(testValue) {
    return function (request, response, next) {
        response.set("X-Test-Response-Header", testValue);
    };
}
