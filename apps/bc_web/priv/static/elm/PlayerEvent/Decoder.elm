module PlayerEvent.Decoder where

import PlayerEvent.Model as PlayerEvent exposing (Model)
import Player.Decode exposing (..)

playerEvent : Decoder PlayerEvent
playerEvent =
    object2 PlayerEvent
        ("event_type" := string)
        ("player" := player)
