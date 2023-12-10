let computation = ./lib/purescript-computation/spago.dhall as Location

let docs = ./lib/purescript-docs/spago.dhall as Location

let docs-sandbox = ./lib/purescript-docs-sandbox/spago.dhall as Location

let json-schema = ./lib/purescript-json-schema/spago.dhall as Location

let json-schema-cli = ./lib/purescript-json-schema-cli/spago.dhall as Location

let json-schema-sandbox =
      ./lib/purescript-json-schema-sandbox/spago.dhall as Location

let markdown = ./lib/purescript-markdown/spago.dhall as Location

let optparse = ./lib/purescript-optparse/spago.dhall as Location

let utils = ./lib/purescript-utils/spago.dhall as Location

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231023/packages.dhall
        sha256:b9a482e743055ba8f2d65b08a88cd772b59c6e2084d0e5ad854025fa90417fd4

in      upstream
    //  { computation
        , docs
        , docs-sandbox
        , json-schema
        , json-schema-cli
        , json-schema-sandbox
        , markdown
        , optparse
        , utils
        }
