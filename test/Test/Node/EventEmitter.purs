module Test.Node.EventEmitter where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Foldable (fold, for_, foldMap)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn1, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Node.EventEmitter (EventEmitter, unsafeEmitFn, unsafeOn, unsafeSubscribe)
import Node.EventEmitter as EventEmitter
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "event-emitter" do
  it "init works" do
    liftEffect $ void $ EventEmitter.new
  it "emit works" do
    liftEffect do
      ee <- EventEmitter.new
      void $ runEffectFn1 (unsafeEmitFn ee :: EffectFn1 String Boolean) "foo"
  it "handle works" do
    liftEffect do
      ee <- EventEmitter.new
      void $ runEffectFn3 unsafeOn ee "foo" $ mkEffectFn1 \_ -> pure unit
      void $ runEffectFn1 (unsafeEmitFn ee) "foo"

  it "subscribe works" do
    let expected = "aaa"
    actual <- liftEffect do
      ref <- Ref.new ""
      ee <- EventEmitter.new
      unsub <- unsafeSubscribe ee "foo" $ mkEffectFn1 \s -> Ref.modify_ (\r -> r <> s) ref
      void $ runEffectFn2 (unsafeEmitFn ee) "foo" expected
      unsub
      void $ runEffectFn2 (unsafeEmitFn ee) "foo" "bbb"
      Ref.read ref
    expected `shouldEqual` actual

  let
    i = 20
    s = "str"
    arr = [ "foo", "bar" ]
  for_ [ 0, 1, 2, 3 ] \argAmt -> do
    it ("emit/handle roundtrips on " <> show argAmt <> " args") do
      ref <- liftEffect $ Ref.new ""
      ee <- liftEffect $ EventEmitter.new
      let
        expected = fold $ Array.take argAmt
          [ show i
          , show s
          , show arr
          ]
      liftEffect $ handleVArgs ee \a b c -> do
        let
          actual = fold
            [ foldMap show a
            , foldMap show b
            , foldMap show c
            ]
        Ref.write actual ref
      liftEffect $ void $ emitVArgs ee
        (i <$ guard (argAmt >= 1))
        (s <$ guard (argAmt >= 2))
        (arr <$ guard (argAmt >= 3))
      actual' <- liftEffect $ Ref.read ref
      expected `shouldEqual` actual'

eventName = "someEvent" :: String

emitVArgs
  :: EventEmitter
  -> Maybe Int
  -> Maybe String
  -> Maybe (Array String)
  -> Effect Boolean
emitVArgs ee = case _, _, _ of
  Just i, Just s, Just a ->
    runEffectFn4 (unsafeEmitFn ee :: EffectFn4 String Int String (Array String) Boolean) eventName i s a
  Just i, Just s, Nothing ->
    runEffectFn3 (unsafeEmitFn ee :: EffectFn3 String Int String Boolean) eventName i s
  Just i, Nothing, Nothing ->
    runEffectFn2 (unsafeEmitFn ee :: EffectFn2 String Int Boolean) eventName i
  _, _, _ ->
    runEffectFn1 (unsafeEmitFn ee :: EffectFn1 String Boolean) eventName

handleVArgs
  :: EventEmitter
  -> (Maybe Int -> Maybe String -> Maybe (Array String) -> Effect Unit)
  -> Effect Unit
handleVArgs ee cb = void do
  runEffectFn3
    -- This could be written
    -- (unsafeOn :: _ _ _ (EffectFn3 (Nullable Code) (Nullable LastStreamId) (Nullable OpaqueData) Unit)) Unit 
    ( unsafeOn
        :: EffectFn3
             EventEmitter
             String
             ( EffectFn3
                 (Nullable Int)
                 (Nullable String)
                 (Nullable (Array String))
                 Unit
             )
             EventEmitter
    )
    ee
    eventName
    (mkEffectFn3 \i s a -> cb (toMaybe i) (toMaybe s) (toMaybe a))
