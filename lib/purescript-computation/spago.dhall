{ name = "purescript-computation"
, dependencies =
  [ "aff"
  , "arrays"
  , "docs"
  , "effect"
  , "foldable-traversable"
  , "heterogeneous"
  , "markdown"
  , "maybe"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "quickcheck"
  , "spec"
  , "strings"
  , "tuples"
  , "utils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
