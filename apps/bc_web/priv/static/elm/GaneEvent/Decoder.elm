module GameEvent.Decoder where

import GameEvent.Model as GameEvent exposing (GameEvent)
import GameStartedEvent.Decoder exposing (..)
import GameErrorEvent.Decoder exposing (..)
import PlayerEvent.Decoder exposing (..)

gameEventInfo : String -> Decoder GameEvent
gameEventInfo eventType =
    case eventType of
        "game_started" ->
            gameStartedEvent
        "game_error" ->
            gameErrorEvent
        _ ->
            playerEvent

gameEvent : Decoder GameEvent
gameEvent =
    at ["game_event", "event_type"] string `andThen` gameEventInfo
