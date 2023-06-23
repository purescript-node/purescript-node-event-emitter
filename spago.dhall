{ name = "node-event-emitters"
, dependencies =
  [ "effect", "either", "functions", "nullable", "prelude", "unsafe-coerce" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
