module Node.EventEmitter.UtilTypes where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4)
import Node.EventEmitter (EventHandle)

type EventHandle0 eventEmitter = EventHandle eventEmitter (Effect Unit) (Effect Unit)
type EventHandle1 eventEmitter a = EventHandle eventEmitter (a -> Effect Unit) (EffectFn1 a Unit)
type EventHandle2 eventEmitter a b = EventHandle eventEmitter (a -> b -> Effect Unit) (EffectFn2 a b Unit)
type EventHandle3 eventEmitter a b c = EventHandle eventEmitter (a -> b -> c -> Effect Unit) (EffectFn3 a b c Unit)
type EventHandle4 eventEmitter a b c d = EventHandle eventEmitter (a -> b -> c -> d -> Effect Unit) (EffectFn4 a b c d Unit)
