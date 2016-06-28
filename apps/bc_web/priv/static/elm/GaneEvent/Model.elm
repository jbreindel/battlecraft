module GameEvent.Model where

import PlayerEvent.Model as PlayerEvent exposing (Model)
import GameStartedEvent.Model as GameStartedEvent exposing (Model)
import GameErrorEvent.Model as GameErrorEvent exposing (Model)

type GameEvent =
    PlayerEvent |
    GameStartedEvent |
    GameErrorEvent
