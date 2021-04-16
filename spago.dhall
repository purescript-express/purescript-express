{ name = "express"
, dependencies =
  [ "aff"
  , "arrays"
  , "effect"
  , "either"
  , "exceptions"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "functions"
  , "maybe"
  , "node-http"
  , "prelude"
  , "psci-support"
  , "strings"
  , "test-unit"
  , "transformers"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
