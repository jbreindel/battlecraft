module GameStartedEvent.Decoder where

import GameStartedEvent.Model as GameStartedEvent exposing (Model)
import Json.Decode exposing (..)
import Player.Decoder exposing (..)

gameStartedEvent : Decoder GameStartedEvent
gameStartedEvent =
    object2 GameStartedEvent
        ("event_type" := string)
        ("players" := list player)
