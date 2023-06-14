{ name = "node-event-emitters-test"
, dependencies =
  [ "aff"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "functions"
  , "prelude"
  , "refs"
  , "spec"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
