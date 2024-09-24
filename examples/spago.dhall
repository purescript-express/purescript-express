let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "integers"
        , "node-process"
        , "refs"
        ]
  , sources =
      conf.sources #
        [ "examples/**/*.purs"
        ]
  }
