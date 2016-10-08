
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
import Spawn
import GameCenter
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
    SpawnMsg Spawn.Msg |
    GameCenterMsg GameCenter.Msg |
    PerformCmd (Cmd Msg) |
    WsReceiveMsg String |
    WsSendMsg String

-- Model

type alias Flags = {
    address : String
}

type alias Model = {
    state : GameState,
    address: String,
    keyboardModel : Keyboard.Model,
    joinModel : Join.Model,
    mapModel : Map.Model,
    spawnModel : Spawn.Model,
    gameCenterModel : GameCenter.Model
}

init : Flags -> Effects Model (Cmd Msg)
init flags =
    let
        (joinModel, joinEffects) = Join.init

        (mapModel, mapEffects) = Map.init

        (spawnModel, spawnEffects) = Spawn.init

        (gameCenterModel, gameCenterEffects) = GameCenter.init

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
            mapModel = mapModel,
            spawnModel = spawnModel,
            gameCenterModel = gameCenterModel
        } [cmdBatch]
        `Effects.andThen` Effects.handle handleJoinEffect joinEffects
        `Effects.andThen` Effects.handle handleMapEffect mapEffects
        `Effects.andThen` Effects.handle handleSpawnEffect spawnEffects
        `Effects.andThen` Effects.handle handleGameCenterEffect gameCenterEffects

-- Update

update : Msg -> Model -> Effects Model (Cmd Msg)
update msg model =
    case msg of

        UpdateGameState state ->
            Effects.return {model | state = state}

        KeyboardMsg keyMsg ->
            onKeyboardMsg keyMsg model

        WindowSize windowSize ->
            onWindowSize windowSize model

        WindowError reason ->
            Effects.return model

        JoinMsg joinMsg ->
            onJoinMsg joinMsg model

        MapMsg mapMsg ->
            onMapMsg mapMsg model

        SpawnMsg spawnMsg ->
            onSpawnMsg spawnMsg model

        GameCenterMsg gameCenterMsg ->
            onGameCenterMsg gameCenterMsg model

        PerformCmd cmd ->
            Effects.init model [cmd]

        WsReceiveMsg str ->
            onWsReceiveMsg str model

        WsSendMsg str ->
            Effects.init model [WebSocket.send model.address str]

onKeyboardMsg : Keyboard.Msg -> Model -> Effects Model (Cmd Msg)
onKeyboardMsg keyboardMsg model =
    let
        (updatedKeyboardModel, keyboardCmd) =
            Keyboard.update keyboardMsg model.keyboardModel

        (updatedMapModel, mapEffects) =
            Map.update (Map.KeyboardMsg updatedKeyboardModel) model.mapModel

        (updatedSpawnModel, spawnEffects) =
            Spawn.update (Spawn.KeyboardMsg updatedKeyboardModel) model.spawnModel
    in
        Effects.init {model |
            keyboardModel = updatedKeyboardModel,
            mapModel = updatedMapModel,
            spawnModel = updatedSpawnModel
        } [Cmd.map KeyboardMsg keyboardCmd]
            `Effects.andThen` Effects.handle handleMapEffect mapEffects
            `Effects.andThen` Effects.handle handleSpawnEffect spawnEffects

onWindowSize : Window.Size -> Model -> Effects Model (Cmd Msg)
onWindowSize windowSize model =
    let
        (updatedMapModel, mapEffects) =
            Map.update (Map.WindowMsg windowSize) model.mapModel
    in
        Effects.return {model |
            mapModel = updatedMapModel
        } `Effects.andThen` Effects.handle handleMapEffect mapEffects

onJoinMsg : Join.Msg -> Model -> Effects Model (Cmd Msg)
onJoinMsg joinMsg model =
    let
        (updateJoinModel, joinEffects) =
            Join.update joinMsg model.joinModel
    in
        Effects.return {model | joinModel = updateJoinModel}
            `Effects.andThen` Effects.handle handleJoinEffect joinEffects

onMapMsg : Map.Msg -> Model -> Effects Model (Cmd Msg)
onMapMsg mapMsg model =
    let
        (updateMapModel, mapEffects) =
            Map.update mapMsg model.mapModel
    in
        Effects.return {model | mapModel = updateMapModel}
            `Effects.andThen` Effects.handle handleMapEffect mapEffects

onSpawnMsg : Spawn.Msg -> Model -> Effects Model (Cmd Msg)
onSpawnMsg spawnMsg model =
    let
        (updateSpawnModel, spawnEffects) =
            Spawn.update spawnMsg model.spawnModel
    in
        Effects.return {model | spawnModel = updateSpawnModel}
            `Effects.andThen` Effects.handle handleSpawnEffect spawnEffects

onGameCenterMsg : GameCenter.Msg -> Model -> Effects Model (Cmd Msg)
onGameCenterMsg gameCenterMsg model =
    let
        (updateGameCenterModel, gameCenterEffects) =
            GameCenter.update gameCenterMsg model.gameCenterModel
    in
        Effects.return {model | gameCenterModel = updateGameCenterModel}
            `Effects.andThen` Effects.handle handleGameCenterEffect gameCenterEffects

onWsReceiveMsg : String -> Model -> Effects Model (Cmd Msg)
onWsReceiveMsg str model =
    case decodeString message str of

        Ok message ->
            onWsReceiveMessage message model

        Err reason ->
            Debug.crash reason
            -- TODO handle error
            -- Effects.return model

onWsReceiveMessage : Message -> Model -> Effects Model (Cmd Msg)
onWsReceiveMessage message model =
    case message of

        JoinResp joinResp ->
            let
                joinMsg = Join.OnJoinResponse joinResp
            in
                update (JoinMsg joinMsg) model

        GameEv gameEv ->
            -- TODO handle game event
            Effects.return model

        EntityEv entityEv ->
            let
                mapMsg = Map.EntityEv entityEv
            in
                update (MapMsg mapMsg) model

        GoldEv goldEv ->
            let
                spawnMsg = Spawn.GoldEv goldEv
            in
                update (SpawnMsg spawnMsg) model

handleJoinEffect : Effects.Handler Join.Effect Model (Cmd Msg)
handleJoinEffect effect model =
    case effect of

        Join.UpdateGameState state ->
            update (UpdateGameState state) model

        Join.WsSendMsg str ->
            update (WsSendMsg str) model

handleMapEffect : Effects.Handler Map.Effect Model (Cmd Msg)
handleMapEffect effect model =
    case effect of

        Map.PerformCmd mapCmdMsg ->
            let
                cmdMsg = Cmd.map MapMsg mapCmdMsg
            in
                update (PerformCmd cmdMsg) model

handleSpawnEffect : Effects.Handler Spawn.Effect Model (Cmd Msg)
handleSpawnEffect effect model =
    case effect of

        Spawn.WsSendMsg str ->
            update (WsSendMsg str) model

handleGameCenterEffect : Effects.Handler GameCenter.Effect Model (Cmd Msg)
handleGameCenterEffect effect model =
    case effect of

        GameCenter.UpdateGameState state ->
            update (UpdateGameState state) model

        GameCenter.PerformCmd gameCenterCmdMsg ->
            let
                cmdMsg = Cmd.map GameCenterMsg gameCenterCmdMsg
            in
                update (PerformCmd cmdMsg) model

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        mapSub = Map.subscriptions model.mapModel
    in
        case model.state of

            GameState.Joining ->
                WebSocket.listen model.address WsReceiveMsg

            _ ->
                Sub.batch [
                    WebSocket.listen model.address WsReceiveMsg,
                    Sub.map KeyboardMsg Keyboard.subscriptions,
                    Sub.map MapMsg mapSub
                ]

-- View

view : Model -> Html Msg
view model =
    let
        body =
            case model.state of

                GameState.Joining ->
                    Join.view model.joinModel
                        |> App.map JoinMsg

                _ ->
                    Map.view model.mapModel
                        |> App.map MapMsg

        spawnContent =
            Spawn.view model.spawnModel
                |> App.map SpawnMsg

        gameCenterContent =
            GameCenter.view model.gameCenterModel
                |> App.map GameCenterMsg
    in
        div [] [
            div [class "game-content"] [
                body
            ],
            spawnContent,
            gameCenterContent
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
