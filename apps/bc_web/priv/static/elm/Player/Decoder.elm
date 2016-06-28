module Player.Decoder where

import Player.Model as Player exposing (Model)
import Json.Decode exposing (..)

player : Decoder Player
player =
    object3 Player
        ("id" := int)
        ("handle" := string)
        ("is_out" := bool)
