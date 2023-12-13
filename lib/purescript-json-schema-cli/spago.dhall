{ name = "purescript-json-schema-cli"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "console"
  , "docs"
  , "docs-sandbox"
  , "dom-indexed"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "json-schema"
  , "markdown"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-child-process"
  , "node-execa"
  , "node-fs"
  , "node-process"
  , "optparse"
  , "ordered-collections"
  , "prelude"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "utils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}