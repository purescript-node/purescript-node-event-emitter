{ name = "node-event-emitters"
, dependencies =
  [ "effect"
  , "either"
  , "functions"
  , "prelude"
  , "unsafe-coerce" 
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
