module Game exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (..)
import Effects exposing (Effects)
import Keyboard.Extra as Keyboard
import Task exposing (Task)
import WebSocket
import Window

-- Local imports

import Join
import Map
import Entity
import GameState exposing (GameState)
import Message exposing (..)

-- Action

type Msg =
    UpdateGameState GameState |
    KeyboardMsg Keyboard.Msg |
    WindowSize Window.Size |
    WindowError String |
    JoinMsg Join.Msg |
    MapMsg Map.Msg |
    PerformCmd (Cmd Msg) |
    WsReceiveMessage String |
    WsSendMessage String

-- Model

type alias Flags = {
    address : String
}

type alias Model = {
    state : GameState,
    address: String,
    keyboardModel : Keyboard.Model,
    joinModel : Join.Model,
    mapModel : Map.Model
}

init : Flags -> Effects Model (Cmd Msg)
init flags =
    let
        (joinModel, joinEffects) = Join.init

        (mapModel, mapEffects) = Map.init

        (keyboardModel, keyboardCmd) = Keyboard.init

        gameKeyboardCmd = Cmd.map KeyboardMsg keyboardCmd

        windowCmd = Task.perform WindowError WindowSize Window.size

        cmdBatch = Cmd.batch [
            gameKeyboardCmd,
            windowCmd
        ]
    in
        Effects.init {
            state = GameState.Joining,
            address = flags.address,
            keyboardModel = keyboardModel,
            joinModel = joinModel,
            mapModel = mapModel
        }
        [cmdBatch]
        `Effects.andThen` Effects.handle handleJoinEffect joinEffects
            `Effects.andThen` Effects.handle handleMapEffect mapEffects

-- Update

update : Msg -> Model -> Effects Model (Cmd Msg)
update msg model =
    case msg of

        UpdateGameState state ->
            Effects.return {model | state = state}

        KeyboardMsg keyMsg ->
            let
                (updatedKeyboardModel, keyboardCmd) =
                    Keyboard.update keyMsg model.keyboardModel

                (updatedMapModel, mapEffects) =
                    Map.update (Map.KeyboardMsg updatedKeyboardModel) model.mapModel
            in
                Effects.init {model |
                    keyboardModel = updatedKeyboardModel,
                    mapModel = updatedMapModel
                } [Cmd.map KeyboardMsg keyboardCmd]
                    `Effects.andThen` Effects.handle handleMapEffect mapEffects

        WindowSize windowSize ->
            let
                (updatedMapModel, mapEffects) =
                    Map.update (Map.WindowMsg windowSize) model.mapModel
            in
                Effects.return {model |
                    mapModel = updatedMapModel
                } `Effects.andThen` Effects.handle handleMapEffect mapEffects

        WindowError reason ->
            Effects.return model

        JoinMsg sub ->
            let
                (updateJoinModel, joinEffects) =
                    Join.update sub model.joinModel
            in
                Effects.return {model | joinModel = updateJoinModel}
                    `Effects.andThen` Effects.handle handleJoinEffect joinEffects

        MapMsg sub ->
            let
                (updateMapModel, mapEffects) =
                    Map.update sub model.mapModel
            in
                Effects.return {model | mapModel = updateMapModel}
                    `Effects.andThen` Effects.handle handleMapEffect mapEffects

        PerformCmd cmd ->
            Effects.init model [cmd]

        WsReceiveMessage str ->
            case decodeString message str of
                Ok message ->
                    onWsReceiveMessage message model
                Err reason ->
                    Debug.crash reason
                    -- TODO handle error
                    -- Effects.return model

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

        EntityEv entityEv ->
            let
                entityMsg = Entity.EntityEv entityEv

                mapMsg = Map.EntityMsg entityMsg
            in
                update (MapMsg mapMsg) model

handleJoinEffect : Effects.Handler Join.Effect Model (Cmd Msg)
handleJoinEffect effect model =
    case effect of

        Join.UpdateGameState state ->
            update (UpdateGameState state) model

        Join.WsSendMessage str ->
            update (WsSendMessage str) model

handleMapEffect : Effects.Handler Map.Effect Model (Cmd Msg)
handleMapEffect effect model =
    case effect of

        Map.PerformCmd mapCmdMsg ->
            let
                cmdMsg = Cmd.map MapMsg mapCmdMsg
            in
                update (PerformCmd cmdMsg) model

        Map.NoOp ->
            Effects.return model

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of

        GameState.Joining ->
            WebSocket.listen model.address WsReceiveMessage

        GameState.Pending ->
            Sub.batch [
                WebSocket.listen model.address WsReceiveMessage,
                Sub.map KeyboardMsg Keyboard.subscriptions
            ]

        GameState.Started ->
            Sub.batch [
                WebSocket.listen model.address WsReceiveMessage,
                Sub.map KeyboardMsg Keyboard.subscriptions
            ]

-- View

view : Model -> Html Msg
view model =
    let
        body =
            case model.state of

                GameState.Joining ->
                    App.map JoinMsg <| Join.view model.joinModel

                GameState.Pending ->
                    App.map MapMsg <| Map.view model.mapModel

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
