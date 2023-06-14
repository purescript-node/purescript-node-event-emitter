module Test.Node.EventEmitter where

import Prelude

import Data.Foldable (for_)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Node.EventEmitter (EventEmitter, EventHandle(..), on, onSubscribe, once, onceSubscribe, prependListener, prependListenerSubscribe, prependOnceListener, prependOnceListenerSubscribe, unsafeEmitFn)
import Node.EventEmitter as EventEmitter
import Node.EventEmitter.UtilTypes (EventHandle1)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

fooHandle :: EventHandle1 EventEmitter String
fooHandle = EventHandle "foo" mkEffectFn1

spec :: Spec Unit
spec = describe "event-emitter" do
  it "`new` does not throw" do
    liftEffect $ void $ EventEmitter.new
  it "`emit` does not throw" do
    liftEffect do
      ee <- EventEmitter.new
      void $ runEffectFn1 (unsafeEmitFn ee :: EffectFn1 String Boolean) "foo"
      void $ runEffectFn2 (unsafeEmitFn ee :: EffectFn2 String String Boolean) "foo" "baz"
      void $ runEffectFn1 (unsafeEmitFn ee) "foo"
      void $ runEffectFn2 (unsafeEmitFn ee) "foo" "bar"
      void $ runEffectFn3 (unsafeEmitFn ee) "foo" "bar" "baz"
  describe "standard functions" do
    let
      fns =
        [ "on" /\ on
        , "once" /\ once
        , "prependListener" /\ prependListener
        , "prependOnceListener" /\ prependOnceListener
        ]
    for_ fns \(fnName /\ fn) -> do
      it (fnName <> " works") do
        liftEffect do
          let expected = "bar"
          ref <- Ref.new ""
          ee <- EventEmitter.new
          fn fooHandle ee \val -> do
            Ref.write val ref
          void $ runEffectFn2 (unsafeEmitFn ee) "foo" expected
          val <- Ref.read ref
          val `shouldEqual` expected

  describe "subscribe functions" do
    let
      fns =
        [ "onSubscribe" /\ onSubscribe
        , "onceSubscribe" /\ onceSubscribe
        , "prependListenerSubscribe" /\ prependListenerSubscribe
        , "prependOnceListenerSubscribe" /\ prependOnceListenerSubscribe
        ]
    for_ fns \(fnName /\ fn) -> do
      it (fnName <> " - normal call works") do
        liftEffect do
          let expected = "bar"
          ref <- Ref.new ""
          ee <- EventEmitter.new
          void $ fn fooHandle ee \val -> do
            Ref.write val ref
          void $ runEffectFn2 (unsafeEmitFn ee) "foo" expected
          val <- Ref.read ref
          val `shouldEqual` expected
    for_ fns \(fnName /\ fn) -> do
      it (fnName <> " - unsubscribing before call works") do
        liftEffect do
          ref <- Ref.new ""
          ee <- EventEmitter.new
          remove <- fn fooHandle ee \val -> do
            Ref.write val ref
          remove
          void $ runEffectFn2 (unsafeEmitFn ee) "foo" "bar"
          val <- Ref.read ref
          val `shouldEqual` ""
