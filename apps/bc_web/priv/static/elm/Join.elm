module Join exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Encode exposing (..)
import WebSocket

import Command exposing (..)

-- Model

type alias Model = {
    address : String,
    handle : String,
    playerId : Int,
    error : Maybe String
}

init : String -> (Model, Cmd Msg)
init address =
    (Model address "" -1 Nothing, Cmd.none)

-- Update

type Msg =
    JoinGame |
    UpdateHandle String |
    OnJoinResponse Response

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        UpdateHandle handle ->
            ({model | handle = handle}, Cmd.none)

        JoinGame ->
            let
                joinCommandJson = initJoinCommand model.handle
                    |> encodeJoinCommand
                    |> encode 0
            in
                (model, WebSocket.send model.address joinCommandJson)

        OnJoinResponse response ->
            case response of
                ResponseErr responseError ->
                    ({model | error = Just responseError.error}, Cmd.none)
                JoinResp joinResponse ->
                    ({model | playerId = joinResponse.playerId}, Cmd.none)

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
