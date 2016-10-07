
module GameCenter exposing (Effect(..),
                            Msg(..),
                            Model,
                            init,
                            update)

import Dict exposing (Dict)
import Time exposing (Time)
import Effects exposing (Effects)

-- Local imports

import GameState exposing (GameState, Started)
import GameEvent exposing (GameEvent)

-- Actions

type Effect =
    UpdateGameState GameState |
    PerformCmd (Cmd Msg)

type Msg = 
    ReceiveGameEv GameEvent |
    AddTimedGameEv (Time, GameEvent) |
    NoOp

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
            
onReceiveGameEvent : GameEvent -> Model -> Effects Model Effect
onReceiveGameEvent gameEvent model =
    let
       cmd =  gameEventCmd gameEvent
    in
        case gameEvent of
    
            PlayerEv playerEvent ->
                let
                    id = updatedPlayer.id
                
                    updatedPlayer = playerEvent.player
                
                    updatedPlayers = 
                        Dict.insert id updatedPlayer model.players
                in
                    Effects.init {model |
                        players = updatedPlayers
                    } [PerformCmd cmd]
                    
            GameStartedEv gameStartedEvent ->
                Effects.init model [
                    PerformCmd cmd,
                    UpdateGameState Started
                ]
                
            _ ->
                Effects.init model [PerformCmd cmd]
            

onAddTimedGameEvent : (Time, GameEvent) -> Model -> Effects Model Effect
onAddTimedGameEvent timeGameEvent model =
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
            Task.succeed (time, playerEvent)
    )
    |> Task.perform NoOp AddGameEv
    