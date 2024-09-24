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
  , "newtype"
  , "node-http"
  , "prelude"
  , "record"
  , "strings"
  , "test-unit"
  , "transformers"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
