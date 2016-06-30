module Join exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (..)
import GameEvent exposing (..)
import WebSocket

-- Model

type alias Model = {
    address : String,
    handle : String,
    playerId : Int
}

init : Model -> (Model, Cmd Msg)
init savedModel =
    (Model savedModel.address "" -1, Cmd.none)

-- Update

type Msg =
    JoinGame |
    UpdateHandle String |
    OnJoinResponse JoinResponse

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        UpdateHandle handle ->
            ({model | handle = handle}, Cmd.none)

        JoinGame ->
            (model, WebSocket.send model.address model.handle)

        OnMessage json ->
            (model, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.address OnMessage

-- View

view : Model -> Html Msg
view model =
    div [class "card"] [
        header [class "card-header"] [
            p [class "card-header-title"] [
                text "Join Game"
            ]
        ],
        div [class "card-content"] [
            div [class "content"] [
                h4 [] [text "Select a handle"],
                input [placeholder "Game Handle", onInput UpdateHandle] []
            ]
        ],
        footer [class "card-footer"] [
            a [class "card-footer-item" onClick JoinGame] [text "Join Game"]
        ]
    ]
