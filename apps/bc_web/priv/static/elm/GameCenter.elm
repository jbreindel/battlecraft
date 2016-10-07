
module GameCenter exposing (Effect(..),
                            Msg(..),
                            Model,
                            init,
                            update)

import Dict exposing (Dict)
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
    NoOp String

-- Model

type alias Model = {
    players : Dict Int Player,
    timedGameEvents : List (Time, GameEvent)
}

init : Effects Model Effect
init =
    Effects.return {
        players = Dict.empty,
        timedGameEvents = []
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
