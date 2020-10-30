// module Node.Express.Middleware.Static
"use strict";

exports._static = function(root) {
  return require("express").static(root)
}
