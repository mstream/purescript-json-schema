{ name = "purescript-json-schema-sandbox"
, dependencies =
  [ "aff"
  , "arrays"
  , "computation"
  , "console"
  , "docs-sandbox"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "json-schema"
  , "json-schema-cli"
  , "markdown"
  , "node-buffer"
  , "node-fs"
  , "ordered-collections"
  , "prelude"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
