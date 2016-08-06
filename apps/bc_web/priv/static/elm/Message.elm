module Message exposing (..)

import Json.Decode exposing (..)

-- Local imports

import Command exposing (..)
import GameEvent exposing (..)
import EntityEvent exposing (..)

-- Aggregate Types

type Message =
    JoinResp JoinResponse |
    GameEv GameEvent |
    EntityEv EntityEvent

messageInfo : String -> Decoder Message
messageInfo messageType =
    case messageType of

        "command_response" ->
            object1 JoinResp joinResponse

        "game_event" ->
            object1 GameEv gameEvent

        "entity_event" ->
            object1 EntityEv entityEvent

        _ ->
            fail <| "Unable to parse messageType: " ++ messageType

message : Decoder Message
message =
    at ["type"] string `andThen` messageInfo
