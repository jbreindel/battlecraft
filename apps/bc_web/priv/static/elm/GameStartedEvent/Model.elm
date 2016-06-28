module GameStartedEvent.Model where

import Player.Model as Player exposing (Model)

type alias Model = {
    eventType : String,
    players : List Player
}

initGameStartedEvent : Model
initGameStartedEvent =
    (Model "" [])
