module Join exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Encode exposing (..)
import WebSocket
import Effects exposing (Effects)

-- Local imports

import Command exposing (..)
import GameState exposing (..)

-- Model

type Effect =
    UpdateGameState GameState |
    WsSendMessage String

type alias Model = {
    handle : String,
    playerId : Int,
    error : Maybe String
}

init : Effects Model (Cmd Msg)
init =
    Effects.return {
        handle = "",
        playerId = -1,
        error = Nothing
    }

-- Update

type Msg =
    JoinGame |
    UpdateHandle String |
    OnJoinResponse Response

update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of

        UpdateHandle handle ->
            Effect.return {model | handle = handle}

        JoinGame ->
            -- TODO check handle length

            let
                joinCommandJson = initJoinCommand model.handle
                    |> encodeJoinCommand
                    |> encode 0
            in
                Effects.init model [WsSendMessage joinCommandJson]

        OnJoinResponse response ->
            case response of
                ResponseErr responseError ->
                    Effects.return {model | error = Just responseError.error}
                JoinResp joinResponse ->
                    Effects.init {model | playerId = joinResponse.playerId}
                        [UpdateGameState Pending]

-- View

view : Model -> Html Msg
view model =
    div [class "card join-card"] [
        header [class "card-header"] [
            p [class "card-header-title"] [
                text "Select a handle"
            ]
        ],
        div [class "card-content"] [
            div [class "content"] [
                input [placeholder "Game Handle", onInput UpdateHandle] []
            ]
        ],
        footer [class "card-footer"] [
            a [class "card-footer-item", onClick JoinGame] [text "Join Game"]
        ]
    ]
