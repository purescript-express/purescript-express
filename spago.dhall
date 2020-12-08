{ name = "express"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "foreign"
  , "foreign-generic"
  , "node-http"
  , "psci-support"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
