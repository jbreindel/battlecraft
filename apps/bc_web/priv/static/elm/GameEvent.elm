
module GameEvent exposing (Player,
                           PlayerEvent,
                           GameStartedEvent,
                           GameErrorEvent,
                           GameEvent(..),
                           gameEvent,
                           player)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)

-- Model types

type alias Player = {
    id : Int,
    handle : String,
    team : Int
}

player : Decoder Player
player =
    succeed Player
        |: ("id" := int)
        |: ("handle" := string)
        |: ("team" := int)

-- GameEvent Types

type alias PlayerEvent = {
    eventType : String,
    player : Player
}

playerEvent : Decoder PlayerEvent
playerEvent =
    at ["game_event"] <| succeed PlayerEvent
        |: ("event_type" := string)
        |: ("player" := player)

type alias GameStartedEvent = {
    eventType : String,
    players : List Player
}

gameStartedEvent : Decoder GameStartedEvent
gameStartedEvent =
    at ["game_event"] <| succeed GameStartedEvent
        |: ("event_type" := string)
        |: ("players" := list player)

type alias GameErrorEvent = {
    eventType : String,
    reason : String
}

gameErrorEvent : Decoder GameErrorEvent
gameErrorEvent =
    at ["game_event"] <| succeed GameErrorEvent
        |: ("event_type" := string)
        |: ("reason" := string)

-- Aggregate Types

type GameEvent =
    PlayerEv PlayerEvent |
    GameStartedEv GameStartedEvent |
    GameErrorEv GameErrorEvent

gameEventInfo : String -> Decoder GameEvent
gameEventInfo eventType =
    case eventType of

        "game_started" ->
            object1 GameStartedEv gameStartedEvent

        "game_error" ->
            object1 GameErrorEv gameErrorEvent

        _ ->
            object1 PlayerEv playerEvent

gameEvent : Decoder GameEvent
gameEvent =
    at ["game_event", "event_type"] string `andThen` gameEventInfo
