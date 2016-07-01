module Join exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import WebSocket

-- Model

-- TODO declare join cmd/response

type alias Model = {
    address : String,
    handle : String,
    playerId : Int
}

init : String -> (Model, Cmd Msg)
init address =
    (Model address "" -1, Cmd.none)

-- Update

type Msg =
    JoinGame |
    UpdateHandle String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        UpdateHandle handle ->
            ({model | handle = handle}, Cmd.none)

        JoinGame ->
            (model, WebSocket.send model.address model.handle)

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
