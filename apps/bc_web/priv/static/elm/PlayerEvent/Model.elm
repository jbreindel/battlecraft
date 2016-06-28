module PlayerEvent.Model where

import Player.Model as Player exposing (Model)
import Player.Model exposing (initPlayer)

type alias Model = {
    eventType : String,
    player : Player
}

initPlayerEvent : Model
initPlayerEvent =
    (Model "" initPlayer)
