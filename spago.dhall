{ name = "mlogo"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "gen"
  , "heterogeneous"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "nonempty"
  , "optparse"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "spec"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources =
  [ "docs/purs/**/*.purs"
  , "src/purs/**/*.purs"
  , "test/snapshot/**/*.purs"
  , "test/unit/purs/**/*.purs"
  ]
}
