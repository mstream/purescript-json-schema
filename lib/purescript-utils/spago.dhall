{ name = "purescript-utils"
, dependencies =
  [ "aff"
  , "console"
  , "datetime"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "pmock"
  , "prelude"
  , "spec"
  , "strings"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
