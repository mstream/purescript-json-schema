let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231023/packages.dhall
        sha256:b9a482e743055ba8f2d65b08a88cd772b59c6e2084d0e5ad854025fa90417fd4
      with open-memoize =
        { dependencies =
          [ "console"
          , "effect"
          , "psci-support"
          , "strings"
          , "lists"
          , "either"
          , "integers"
          , "lazy"
          , "maybe"
          , "partial"
          , "prelude"
          , "tuples"
          ]
        , repo =
            "https://github.com/purescript-open-community/purescript-open-memoize.git"
        , version = "v6.1.0"
        }

in  upstream
