{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "src"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "canvas"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "nonempty"
  , "numbers"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "simple-json"
  , "strings"
  , "tuples"
  , "unfoldable"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
