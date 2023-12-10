{ name = "purescript-docs"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "markdown"
  , "maybe"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "strings"
  , "utils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
