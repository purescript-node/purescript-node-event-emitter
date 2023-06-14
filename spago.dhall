{ name = "node-event-emitters"
, dependencies =
  [ "effect", "either", "exceptions", "functions", "prelude", "unsafe-coerce" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
