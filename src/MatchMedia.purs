module MatchMedia
  ( MatchMedia
  , getMatchMedia
  , matches
  , addListener
  , callListener
  , eventListener
  , fromEvent
  ) where

import Prelude

import Control.Monad.List.Trans (mapMaybe)
import Effect (Effect)
import Halogen.HTML (a)

import Web.Event.EventTarget as EventTarget
import Web.Event.Event as Event
import Data.Foldable (traverse_)
import Data.Maybe (Maybe, Maybe(..))
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)

foreign import data MatchMedia :: Type

foreign import getMatchMedia :: String -> Effect MatchMedia

foreign import matches :: MatchMedia -> Effect Boolean

foreign import addListener :: EventTarget.EventListener -> MatchMedia -> Effect Unit

foreign import callListener :: EventTarget.EventListener -> MatchMedia -> Effect Unit

eventListener :: forall a. MatchMedia -> (Event.Event -> Maybe a) -> HS.Emitter a
eventListener mm f =
  HS.makeEmitter \push -> do
    listener <- EventTarget.eventListener \mm' -> traverse_ push (f mm')
    addListener listener mm
    callListener listener mm
    pure $ pure unit

fromEvent :: Event.Event -> Maybe MatchMedia
fromEvent = Just <<< unsafeCoerce