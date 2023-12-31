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
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-node"
  , "affjax-web"
  , "argonaut"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "dotenv"
  , "effect"
  , "either"
  , "fetch"
  , "fetch-yoga-json"
  , "foreign"
  , "http-methods"
  , "integers"
  , "maybe"
  , "node-process"
  , "prelude"
  , "simple-json"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-socket"
  , "web-xhr"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
