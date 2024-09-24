let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220525/packages.dhall sha256:5facfdf9c35801a0e6a41b08b4293f947743007a9224a2a3d7694d87a44a7f28

let additions =
      { foreign-generic =
          { repo =
              "https://github.com/working-group-purescript-es/purescript-foreign-generic.git"
          , version =
              "v0.15.0-updates"
          , dependencies =
              [ "effect", "foreign", "foreign-object" ]
          }
      }

in  additions ⫽ upstream
