{ name = "purescript-markdown"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "spec"
  , "strings"
  , "tuples"
  , "unfoldable"
  , "utils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
