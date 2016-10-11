
module GameEvent exposing (Player,
                           PlayerEvent,
                           GamePlayerEvent(..),
                           GameStartedEvent,
                           GameErrorEvent,
                           GameEvent(..),
                           gameEvent,
                           playerEvent,
                           player,
                           gamePlayerEventPlayer)

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

-- GamePlayerEvent Types

type alias PlayerEvent = {
    eventType : String,
    player : Player
}

playerEvent : Decoder PlayerEvent
playerEvent =
    at ["game_event"] <| succeed PlayerEvent
        |: ("event_type" := string)
        |: ("player" := player)

type GamePlayerEvent =
    PlayerJoinedEv PlayerEvent |
    PlayerQuitEv PlayerEvent |
    PlayerOutEv PlayerEvent

gamePlayerEventInfo : String -> Decoder GamePlayerEvent
gamePlayerEventInfo eventType =
    case eventType of

        "player_joined" ->
            object1 PlayerJoinedEv playerEvent

        "player_quit" ->
            object1 PlayerQuitEv playerEvent

        "player_out" ->
            object1 PlayerOutEv playerEvent

        _ ->
            object1 PlayerOutEv playerEvent

gamePlayerEvent : Decoder GamePlayerEvent
gamePlayerEvent =
    at ["game_event", "event_type"] string
        `andThen` gamePlayerEventInfo

-- GameEvent Types

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

type alias GameWonEvent = {
    eventType : String,
    winners : List Player
}

gameWonEvent : Decoder GameWonEvent
gameWonEvent =
    at ["game_event"] <| succeed GameWonEvent
        |: ("event_type" := string)
        |: ("winners" := list player)

-- Aggregate Types

type GameEvent =
    GamePlayerEv GamePlayerEvent |
    GameStartedEv GameStartedEvent |
    GameErrorEv GameErrorEvent |
    GameWonEv GameWonEvent

gameEventInfo : String -> Decoder GameEvent
gameEventInfo eventType =
    case eventType of

        "game_started" ->
            object1 GameStartedEv gameStartedEvent

        "game_error" ->
            object1 GameErrorEv gameErrorEvent

        "game_won" ->
            object1 GameWonEv gameWonEvent

        _ ->
            object1 GamePlayerEv gamePlayerEvent

gameEvent : Decoder GameEvent
gameEvent =
    at ["game_event", "event_type"] string
        `andThen` gameEventInfo

-- Helper Funs

gamePlayerEventPlayer : GamePlayerEvent -> Player
gamePlayerEventPlayer gamePlayerEvent =
    case gamePlayerEvent of

        PlayerJoinedEv playerJoinedEvent ->
            playerJoinedEvent.player

        PlayerQuitEv playerQuitEvent ->
            playerQuitEvent.player

        PlayerOutEv playerOutEvent ->
            playerOutEvent.player
