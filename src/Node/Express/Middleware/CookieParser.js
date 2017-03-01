// module Node.Express.Middleware.CookieParser
"use strict";

exports._cookieParser = function(req, res, nxt) {
  return function() {
    var cp = require("cookie-parser");
    return cp()(req, res, nxt);
  }
}

exports._secretCookieParser = function(secret) {
  return function(req, res, nxt) {
    return function() {
      var cp = require("cookie-parser");
      return cp(secret)(req, res, nxt)
    }
  }
}
