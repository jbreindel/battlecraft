module GameEvent exposing (..)

import Json.Decode exposing (..)

-- Model types

type alias Player = {
    id : Int,
    handle : String,
    isOut : Bool
}

player : Decoder Player
player =
    object3 Player
        ("id" := int)
        ("handle" := string)
        ("is_out" := bool)

-- GameEvent Types

type alias PlayerEvent = {
    eventType : String,
    player : Player
}

playerEvent : Decoder PlayerEvent
playerEvent =
    object2 PlayerEvent
        ("event_type" := string)
        ("player" := player)

type alias GameStartedEvent = {
    eventType : String,
    players : List Player
}

gameStartedEvent : Decoder GameStartedEvent
gameStartedEvent =
    object2 GameStartedEvent
        ("event_type" := string)
        ("players" := list player)

type alias GameErrorEvent = {
    eventType : String,
    reason : String
}

gameErrorEvent : Decoder GameErrorEvent
gameErrorEvent =
    object2 GameErrorEvent
        ("event_type" := string)
        ("reason" := string)

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
