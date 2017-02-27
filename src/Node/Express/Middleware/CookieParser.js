// module Node.Express.Middleware.CookieParser
"use strict";

exports._cookieParser = function(req, res, nxt) {
  return function() {
    var cp = require("cookie-parser");
    return cp()(req, res, nxt);
  }
}

exports._secretCookieParser = function(req, res, nxt) {
  return function() {
    var cp = require("cookie-parser");
    return cp(true)(req, res, nxt)
  }
}
