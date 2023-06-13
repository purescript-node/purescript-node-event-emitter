module Node.EventEmitter
  ( EventEmitter
  , new
  , unsafeEmitFn
  , JsSymbol
  , eventNames
  , getMaxListeners
  , listenerCount
  , unsafeOff
  , unsafeOn
  , unsafeSubscribe
  , unsafePrependListener
  , unsafePrependSubscribe
  , unsafePrependOnceListener
  , setMaxListeners
  , setUnlimitedListeners
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

foreign import data EventEmitter :: Type

-- | Create a new event emitter
foreign import new :: Effect EventEmitter

-- | THIS IS UNSAFE! REALLY UNSAFE!
-- | Gets the `emit` function for a particular `EventEmitter`.
-- | Intended usage is to prevent the need to write redundant FFI:
-- |
-- | `http2session.goaway([code[, lastStreamID[, opaqueData]]])` as an example...
-- | - https://nodejs.org/dist/latest-v18.x/docs/api/http2.html#event-goaway
-- | - https://nodejs.org/dist/latest-v18.x/docs/api/http2.html#http2sessiongoawaycode-laststreamid-opaquedata
-- |
-- | We can then write a single function that handles all four cases:
-- | ```
-- | goAway
-- |   :: Http2Session
-- |   -> Maybe Code
-- |   -> Maybe LastStreamId
-- |   -> Maybe OpaqueData
-- |   -> Effect Unit
-- | goAway h2s = case _, _, _ of
-- |   Just c, Just id, Just d ->
-- |     runEffectFn4 (unsafeEmitFn h2s :: EffectFn4 String Code LastStreamId OpaqueData Unit) "goaway" c id d
-- |   Just c, Just id, Nothing ->
-- |     -- If you're feeling lucky, omit the type annotations completely
-- |     runEffectFn3 (unsafeEmitFn h2s) "goaway" c id
-- |   Just c, Nothing, Nothing ->
-- |     runEffectFn2 (unsafeEmitFn h2s :: EffectFn2 String Code LastStreamId Unit) "goaway" c
-- |   _, _, _ ->
-- |     runEffectFn1 (unsafeEmitFn h2s :: EffectFn1 String Unit) "goaway"
-- | ```
-- | 
-- | Synchronously calls each of the listeners registered for the event named `eventName`, in the order they were registered, passing the supplied arguments to each.
-- | Returns `true` if the event had listeners, `false` otherwise.
foreign import unsafeEmitFn :: forall f. EventEmitter -> f Boolean

-- | THIS IS UNSAFE! REALLY UNSAFE!
-- | Intended usage is to prevent the need to write redundant FFI:
-- |
-- | `http2session.goaway([code[, lastStreamID[, opaqueData]]])` as an example...
-- | - https://nodejs.org/dist/latest-v18.x/docs/api/http2.html#event-goaway
-- | - https://nodejs.org/dist/latest-v18.x/docs/api/http2.html#http2sessiongoawaycode-laststreamid-opaquedata
-- |
-- | We can then write a single type-safe idiomatic function that handles all four cases:
-- | ```
-- | onGoAway
-- |   :: Http2Session
-- |   -> (Maybe Code -> Maybe LastStreamId -> Maybe OpaqueData -> Effect Unit)
-- |   -> Effect Void
-- | onGoAway h2s cb = void $
-- |   runEffectFn3 
-- |     -- If you're feeling lucky, omit the type annotations on `unsafeOn` completely.
-- |     (unsafeOn :: EffectFn3 EventEmitter String (EffectFn3 (Nullable Code) (Nullable LastStreamId) (Nullable OpaqueData) Unit)) EventEmitter 
-- |     (unsafeCoerce h2s :: EventEmitter)
-- |     "goaway" 
-- |     (mkEffectFn3 \c id d -> cb (toMaybe c) (toMaybe id) (toMaybe d))
-- | ```
foreign import unsafeOn :: forall f. EffectFn3 EventEmitter String f Unit

foreign import unsafeOff :: forall f. EffectFn3 EventEmitter String f Unit

unsafeSubscribe :: forall f. EventEmitter -> String -> f -> Effect (Effect Unit)
unsafeSubscribe ee eventName f = do
  runEffectFn3 unsafeOn ee eventName f
  pure $ runEffectFn3 unsafeOff ee eventName f

foreign import unsafeOnce :: forall f. EffectFn3 EventEmitter String f Unit
foreign import unsafePrependListener :: forall f. EffectFn3 EventEmitter String f Unit

unsafePrependSubscribe :: forall f. EventEmitter -> String -> f -> Effect (Effect Unit)
unsafePrependSubscribe ee eventName f = do
  runEffectFn3 unsafePrependListener ee eventName f
  pure $ runEffectFn3 unsafeOff ee eventName f

foreign import unsafePrependOnceListener :: forall f. EffectFn3 EventEmitter String f EventEmitter

foreign import data StrOrSymbol :: Type

foreign import eventNamesImpl :: EventEmitter -> Array StrOrSymbol

foreign import data JsSymbol :: Type

eventNames :: EventEmitter -> Array (Either JsSymbol String)
eventNames ee = map (\x -> runFn3 strOrSymbol Left Right x) $ eventNamesImpl ee

foreign import strOrSymbol :: Fn3 (forall a. JsSymbol -> Either JsSymbol a) (forall b. String -> Either b String) StrOrSymbol (Either JsSymbol String)

foreign import getMaxListenersImpl :: EffectFn1 EventEmitter Int

getMaxListeners :: EventEmitter -> Effect Int
getMaxListeners = runEffectFn1 getMaxListenersImpl

foreign import listenerCountImpl :: EffectFn2 EventEmitter String Int

listenerCount :: EventEmitter -> String -> Effect Int
listenerCount = runEffectFn2 listenerCountImpl

setMaxListeners :: Int -> EventEmitter -> Effect Unit
setMaxListeners max emitter = runEffectFn2 setMaxListenersImpl emitter max

setUnlimitedListeners :: EventEmitter -> Effect Unit
setUnlimitedListeners = setMaxListeners 0

foreign import setMaxListenersImpl :: EffectFn2 (EventEmitter) Int Unit

