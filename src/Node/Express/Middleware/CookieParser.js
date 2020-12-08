// module Node.Express.Middleware.CookieParser
"use strict";

// we could write this middleware as
// `exports._cookieParser = require("cookie-parser")()`
// but this variant is better for tree-shaking
exports._cookieParser = function(req, res, nxt) {
  var cp = require("cookie-parser");
  return cp()(req, res, nxt);
}
