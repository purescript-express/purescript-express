// module Node.Express.Middleware.Static
"use strict";

exports._static = function(root) {
  return function(req, res, nxt) {
    return function() {
      var express = require("express");
      return express.static(root)(req, res, nxt);
    }
  }
}
