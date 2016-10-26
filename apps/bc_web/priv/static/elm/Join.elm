
module Join exposing (Effect(..),
                      Msg(..),
                      Model,
                      init,
                      update,
                      view)

import String as String
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Html.Events exposing (onInput, onClick)
import Json.Encode exposing (..)
import Effects exposing (Effects)
import WebSocket

-- Local imports

import Command exposing (..)
import GameState exposing (..)

-- Actions

type Effect =
    UpdateGameState GameState |
    WsSendMsg String

type Msg =
    JoinGame |
    UpdateHandle String |
    OnJoinResponse JoinResponse

-- Model

type alias Model = {
    handle : String,
    playerId : Int,
    error : Maybe String
}

init : Effects Model Effect
init =
    Effects.return {
        handle = "",
        playerId = -1,
        error = Nothing
    }

-- Update

update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of

        UpdateHandle handle ->
            Effects.return {model |
                                handle = handle,
                                error = Nothing}

        JoinGame ->
            onJoinGame model

        OnJoinResponse joinResponse ->
            onJoinResponse joinResponse model

onJoinGame : Model -> Effects Model Effect
onJoinGame model =
    let
        handleLength = String.length model.handle
    in
        if (not (handleLength > 3)) || (handleLength > 8) then
            Effects.return {model |
                error = Just "Handle has to be between 3 and 8 characters long"}
        else
            let
                joinCommandJson = initJoinCommand model.handle
                    |> encodeJoinCommand
                    |> encode 0
            in
                Effects.init {model |
                    error = Nothing
                } [WsSendMsg joinCommandJson]

onJoinResponse : JoinResponse -> Model -> Effects Model Effect
onJoinResponse joinResponse model =
    case joinResponse of

        JoinErr joinErrorResp ->
            Effects.return {model |
                                error = Just joinErrorResp.error}

        JoinSucc joinSuccResp ->
            Effects.init {model |
                playerId = joinSuccResp.playerId
            } [UpdateGameState Pending]

-- View

view : Model -> Html Msg
view model =
    lazy joinCard model

joinCard : Model -> Html Msg
joinCard model =
    let
        errorColumnsClass =
            case model.error of

                Just _ ->
                    "columns"

                Nothing ->
                    "columns is-hidden"

        errorMessage = Maybe.withDefault "" model.error
    in
        div [class "card join-card"] [
            header [class "card-header"] [
                p [class "card-header-title"] [
                    text "Select a handle"
                ]
            ],
            div [class "card-content"] [
                div [class "content"] [
                    div [class errorColumnsClass] [
                        div [class "column"] [
                            article [class "message is-danger"] [
                                div [class "message-body"] [
                                    text errorMessage
                                ]
                            ]
                        ]
                    ],
                    div [class "columns"] [
                        div [class "column"] [
                            input [placeholder "Game Handle", onInput UpdateHandle] []
                        ]
                    ]
                ]
            ],
            footer [class "card-footer"] [
                a [class "card-footer-item", onClick JoinGame] [text "Join Game"]
            ]
        ]
