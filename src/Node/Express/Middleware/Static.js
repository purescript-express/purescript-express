// module Node.Express.Middleware.Static
"use strict";

exports._static = function(root) {
  return function(req, res, nxt) {
    return function() {
      var app = require("express");
      return app.static(root)(req, res, nxt);
    }
  }
}
