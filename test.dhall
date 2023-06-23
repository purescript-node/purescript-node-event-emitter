let config = ./spago.dhall

-- This hack only works because I know how spago@0.20.9 works.
-- When we run `spago -x test.dhall install`, the package(s) installed
-- will be added to the `spagoHack` dependencies array, but Spago
-- will say the config file was not updated. It's lying.
-- Then when we run `spago -x test.dhall build`,
-- the final expression in the `in` block will be used.
let spagoHack =
      { name = "node-event-emitters-test"
      , dependencies =
        [ "aff", "foldable-traversable", "maybe", "refs", "spec", "tuples" ]
      , packages = ./packages.dhall
      , sources = [ "test/**/*.purs" ]
      }

in  { name = spagoHack.name
    , dependencies = spagoHack.dependencies # config.dependencies
    , packages = ./packages.dhall
    , sources = spagoHack.sources # config.sources
    }
