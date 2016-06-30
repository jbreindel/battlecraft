port module Game exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (..)
import GameEvent exposing (..)
import WebSocket
import Debug exposing (log)
        ]

-- Main

main : Program Model
main =
    App.programWithFlags {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }
