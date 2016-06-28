module GameErrorEvent.Decoder where

import GameErrorEvent.Model as GameErrorEvent exposing (Model)
import Json.Decode exposing (..)

gameErrorEvent : Decoder GameErrorEvent
gameErrorEvent =
    object2 GameErrorEvent
        ("event_type" := string)
        ("reason" := string)
