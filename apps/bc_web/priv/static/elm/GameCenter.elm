
module GameCenter exposing (Effect(..),
                            Msg(..),
                            Model,
                            init,
                            update,
                            view)

import Html exposing (Html, div, button, h5, p,
                        table, tbody, tr, td, small, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Dict exposing (Dict)
import List.Extra as ListExtra
import Time exposing (Time)
import Task exposing (Task)
import Effects exposing (Effects)

-- Local imports

import GameState exposing (GameState)
import GameEvent exposing (GameEvent,
                           Player,
                           PlayerEvent,
                           gamePlayerEventPlayer)

-- Actions

type Effect =
    UpdateGameState GameState |
    PerformCmd (Cmd Msg)

type Msg =
    ReceiveGameEv GameEvent |
    AddTimedGameEv (Time, GameEvent) |
    Minimize |
    NoOp String

-- Model

type alias Model = {
    players : Dict Int Player,
    timedGameEvents : List (String, String),
    minimized : Bool
}

init : Effects Model Effect
init =
    Effects.return {
        players = Dict.empty,
        timedGameEvents = [],
        minimized = False
    }

-- Update

update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of

        ReceiveGameEv gameEvent ->
            onReceiveGameEvent gameEvent model

        AddTimedGameEv timedGameEvent ->
            onAddTimedGameEvent timedGameEvent model

        Minimize ->
            onMinimize model

        _ ->
            Effects.return model

onReceiveGameEvent : GameEvent -> Model -> Effects Model Effect
onReceiveGameEvent gameEvent model =
    let
       cmd =  gameEventCmd gameEvent
    in
        case gameEvent of

            GameEvent.GamePlayerEv gamePlayerEvent ->
                let
                    id = updatedPlayer.id

                    updatedPlayer =
                        gamePlayerEventPlayer gamePlayerEvent

                    updatedPlayers =
                        Dict.insert id updatedPlayer model.players
                in
                    Effects.init {model |
                        players = updatedPlayers
                    } [PerformCmd cmd]

            GameEvent.GameStartedEv gameStartedEvent ->
                let
                    updatedPlayerDict = playerDict gameStartedEvent.players
                in
                    Effects.init {model |
                                    players = updatedPlayerDict} [
                        PerformCmd cmd,
                        UpdateGameState GameState.Started
                    ]

            GameEvent.GameWonEv gameWonEvent ->
                -- TODO more with winners
                let
                    updatedPlayerDict = playerDict gameWonEvent.winners
                in
                    Effects.init {model |
                                    players = updatedPlayerDict} [
                        PerformCmd cmd,
                        UpdateGameState GameState.Won
                    ]

            _ ->
                Effects.init model [PerformCmd cmd]


onAddTimedGameEvent : (Time, GameEvent) -> Model -> Effects Model Effect
onAddTimedGameEvent (time, gameEvent) model =
    let
        timeMessage = timestampLogMessage time

        logMessage = gameEventLogMessage gameEvent

        updatedTimedGameEvents =
            model.timedGameEvents ++ [(timeMessage, logMessage)]
    in
        Effects.return {model |
            timedGameEvents = updatedTimedGameEvents
        }

onMinimize : Model -> Effects Model Effect
onMinimize model =
    let
        minimized =
            if model.minimized then
                False
            else
                True
    in
        Effects.return {model | minimized = minimized}

gameEventCmd : GameEvent -> Cmd Msg
gameEventCmd gameEvent =
    Time.now `Task.andThen` (
        \time ->
            Task.succeed (time, gameEvent)
    )
    |> Task.perform NoOp AddTimedGameEv

playerDict : List Player -> Dict Int Player
playerDict players =
    players
        |> List.map (
            \player ->
                (player.id, player)
        )
        |> Dict.fromList

timestampLogMessage : Time -> String
timestampLogMessage time =
    toString time

gameEventLogMessage : GameEvent -> String
gameEventLogMessage gameEvent =
    case gameEvent of

        GameEvent.GameStartedEv gameStartedEvent ->
            "Game has started"

        GameEvent.GameErrorEv gameErrorEvent ->
            "A game error has occured"

        GameEvent.GameWonEv gameWonEvent ->
            "Game has been won"

        GameEvent.GamePlayerEv gamePlayerEvent ->
            gamePlayerEventLogMessage gamePlayerEvent

gamePlayerEventLogMessage : GameEvent.GamePlayerEvent -> String
gamePlayerEventLogMessage gamePlayerEvent =
    let
        player = gamePlayerEventPlayer gamePlayerEvent

        handle = player.handle
    in
        case gamePlayerEvent of

            GameEvent.PlayerJoinedEv playerJoinedEvent ->
                handle ++ " has joined the game"

            GameEvent.PlayerQuitEv playerQuitEvent ->
                handle ++ " has quit the game"

            GameEvent.PlayerOutEv playerOutEvent ->
                handle ++ " has been knocked out"

-- View

view : Model -> Html Msg
view model =
    lazy gameCenterContent model

gameCenterContent : Model -> Html Msg
gameCenterContent model =
    let
        minimizedColumnClass =
            if model.minimized then
                "columns is-hidden"
            else
                "columns"
    in
        div [class "is-overlay game-center-content"] [
            div [class "notification"] [
                button [class "delete", onClick Minimize] [],

                -- Heading
                div [class "columns"] [
                    gameCenterTitleColumn model
                ],

                -- Players
                div [class minimizedColumnClass] [
                    playersTableColumn model
                ],

                -- Log
                div [class minimizedColumnClass] [
                    logColumn model
                ]
            ]
        ]

gameCenterTitleColumn : Model -> Html Msg
gameCenterTitleColumn model =
    div [class "column is-flex is-vcentered"] [
        h5 [class "title is-5"] [
            text "Game Center"
        ]
    ]

playersTableColumn : Model -> Html Msg
playersTableColumn model =
    let
        tableRows =
            model.players
                |> Dict.values
                |> ListExtra.groupsOf 2
                |> List.map (
                    \players ->
                        List.map (
                            \player ->
                                td [] [
                                    text player.handle
                                ]
                        ) players
                )
                |> List.foldl (
                    \playerCells playerRows ->
                        playerRows ++ [
                            tr [] playerCells
                        ]
                ) []
    in
        div [class "column"] [
            table [class "table is-bordered"] [
                tbody [] tableRows
            ]
        ]

logColumn : Model -> Html Msg
logColumn model =
    let
        logMessages =
            model.timedGameEvents
                |> List.map (
                    \(time, logMessage) ->
                        p [] [
                            small [] [text time],
                            text (" - " ++ logMessage)
                        ]
                )
    in
        div [class "column box log"] logMessages
