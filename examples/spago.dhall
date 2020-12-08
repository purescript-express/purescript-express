{ name = "express-examples"
, dependencies =
  (../spago.dhall).dependencies # [ "console", "effect", "node-fs", "psci-support", "refs", "strings", "node-process" ]
, packages = (../spago.dhall).packages
, sources = (../spago.dhall).sources # ["examples/src/**/*.purs"]
}
