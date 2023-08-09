{ name = "mlogo"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "arrays"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "gen"
  , "lists"
  , "maybe"
  , "node-process"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "quickcheck"
  , "spec"
  , "strings"
  , "tailrec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/purs/**/*.purs", "test/unit/**/*.purs" ]
}
