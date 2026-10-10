module Test.Main where

import Prelude

import Effect (Effect)
import Test.Node.EventEmitter as EventEmitter
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] EventEmitter.spec
