let upstream =
      ../../packages.dhall
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
