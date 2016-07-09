port module Game exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (..)
import WebSocket
import Effects exposing (Effects)

-- Local imports

import Join
import GameState exposing (GameState)
import Message exposing (..)

-- Action

type Msg =
    UpdateGameState GameState |
    JoinMsg Join.Msg |
    WsReceiveMessage String |
    WsSendMessage String

-- Model

type alias Flags = {
    address : String
}

type alias Model = {
    state : GameState,
    address: String,
    joinModel : Join.Model
}

init : Flags -> Effects Model (Cmd Msg)
init flags =
    let
        (joinModel, joinEffects) = Join.init
    in
        Effects.return {
            state = GameState.Joining,
            address = flags.address,
            joinModel = joinModel
        } `Effects.andThen` Effects.handle handleJoinEffect joinEffects

-- Update

update : Msg -> Model -> Effects Model (Cmd Msg)
update msg model =
    case msg of

        UpdateGameState state ->
            Effects.return {model | state = state}

        JoinMsg sub ->
            let
                (updateJoinModel, joinEffects) =
                    Join.update sub model.joinModel
            in
                Effects.return {model | joinModel = updateJoinModel}
                    `Effects.andThen` Effects.handle handleJoinEffect joinEffects

        WsReceiveMessage str ->
            case decodeString message str of
                Ok message ->
                    onWsReceiveMessage message model
                Err reason ->
                    -- TODO handle error
                    Effects.return model

        WsSendMessage str ->
            Effects.init model [WebSocket.send model.address str]

onWsReceiveMessage : Message -> Model -> Effects Model (Cmd Msg)
onWsReceiveMessage message model =
    case message of
        JoinResp joinResp ->
            update (JoinMsg (Join.OnJoinResponse joinResp)) model
        GameEv gameEv ->
            -- TODO handle game event
            Effects.return model

handleJoinEffect : Effects.Handler Join.Effect Model (Cmd Msg)
handleJoinEffect effect model =
    case effect of
        Join.UpdateGameState state ->
            update (UpdateGameState state) model

        Join.WsSendMessage str ->
            update (WsSendMessage str) model

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.address WsReceiveMessage

-- View

view : Model -> Html Msg
view model =
    let
        body =
            case model.state of

                GameState.Joining ->
                    App.map JoinMsg <| Join.view model.joinModel

                GameState.Pending ->
                    -- TODO create pending view
                    div [] []

                GameState.Started ->
                    -- TODO create map view
                    div [] []

    in
        div [class "game-content is-full-width"] [
            body
        ]

-- Main

main : Program Flags
main =
    App.programWithFlags {
        init = \flags ->
                    let
                        (model, effects) = init flags
                    in
                        (model, effects) |> Effects.toCmd,
        view = view,
        update = \msg model -> update msg model |> Effects.toCmd,
        subscriptions = subscriptions
    }
