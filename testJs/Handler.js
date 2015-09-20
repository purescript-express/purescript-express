
var Handler = function(method, route, useOnError, fn) {
    this.method = method;
    this.route = route;
    this.useOnError = useOnError;
    if (useOnError) {
        this.run = function(app, error, req, resp, next) {
            return fn.apply(app, [error, req, resp, next]);
        };
    } else {
        this.run = function(app, req, resp, next) {
            return fn.apply(app, [req, resp, next]);
        };
    }
}

Handler.prototype.matches = function(request) {
    return this.methodMatches(request)
        && this.routeMatches(request)
        && !this.useOnError;
}

Handler.prototype.methodMatches = function(request) {
    return this.method == null || this.method == request.method;
}

Handler.prototype.routeMatches = function(request) {
    return this.route == null || request.path.search(this.route) == 0;
}

module.exports = Handler;
