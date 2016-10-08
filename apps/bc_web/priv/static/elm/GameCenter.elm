
module GameCenter exposing (Effect(..),
                            Msg(..),
                            Model,
                            init,
                            update,
                            view)

import Html exposing (Html, div, button, h5, p, table, tbody, tr, td, text)
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
import GameEvent exposing (GameEvent, Player, PlayerEvent)

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
    timedGameEvents : List (Time, GameEvent),
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

        _ ->
            Effects.return model

onReceiveGameEvent : GameEvent -> Model -> Effects Model Effect
onReceiveGameEvent gameEvent model =
    let
       cmd =  gameEventCmd gameEvent
    in
        case gameEvent of

            GameEvent.PlayerEv playerEvent ->
                let
                    id = updatedPlayer.id

                    updatedPlayer = playerEvent.player

                    updatedPlayers =
                        Dict.insert id updatedPlayer model.players
                in
                    Effects.init {model |
                        players = updatedPlayers
                    } [PerformCmd cmd]

            GameEvent.GameStartedEv gameStartedEvent ->
                Effects.init model [
                    PerformCmd cmd,
                    UpdateGameState GameState.Started
                ]

            _ ->
                Effects.init model [PerformCmd cmd]


onAddTimedGameEvent : (Time, GameEvent) -> Model -> Effects Model Effect
onAddTimedGameEvent timedGameEvent model =
    let
        updatedTimedGameEvents =
            model.timedGameEvents ++ [timedGameEvent]
    in
        Effects.return {model |
            timedGameEvents = updatedTimedGameEvents
        }

gameEventCmd : GameEvent -> Cmd Msg
gameEventCmd gameEvent =
    Time.now `Task.andThen` (
        \time ->
            Task.succeed (time, gameEvent)
    )
    |> Task.perform NoOp AddTimedGameEv

-- View

view : Model -> Html Msg
view model =
    lazy gameCenterContent model

gameCenterContent : Model -> Html Msg
gameCenterContent model =
    div [class "is-overlay game-center-content"] [
        div [class "notification"] [
            button [class "delete", onClick Minimize] [],

            -- Heading
            div [class "columns"] [
                gameCenterTitleColumn model
            ],

            -- Players
            div [class "columns"] [
                playersTableColumn model
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
