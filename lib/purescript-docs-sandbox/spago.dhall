{ name = "purescript-docs-sandbox"
, dependencies =
  [ "aff"
  , "console"
  , "dom-indexed"
  , "effect"
  , "exceptions"
  , "halogen"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
