port module Join exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (..)
import Message
import WebSocket
import Debug exposing (log)

-- Model

type alias Model = {
    address : String,
    handle : String,
    playerId : Int
}

init : Model -> (Model, Cmd Msg)
init savedModel =
    (Model savedModel.address savedModel.handle -1, Cmd.none)

-- Update

type Msg =
    JoinGame |
    UpdateHandle String |
    OnMessage String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        UpdateHandle handle ->
            ({model | handle = handle}, Cmd.none)

        JoinGame ->
            (model, WebSocket.send model.address model.handle)

        OnMessage json ->
            (model, Cmd.none)

{--

            case decodeString Message.message json of
                Ok Message ->
                    -- TODO decompose join message
                    log "websocket" Message
                Err Reason ->
                    -- TODO show error message
                    log "error" Reason

--}

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.address OnMessage

-- View

view : Model -> Html Msg
view model =
    div []
        [
            input [placeholder "Game Handle", onInput UpdateHandle] [],
            button [onClick JoinGame] [text "Join"]
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
