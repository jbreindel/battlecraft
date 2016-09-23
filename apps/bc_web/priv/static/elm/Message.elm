
module Message exposing (Message(..),
                         message)

import Json.Decode exposing (..)

-- Local imports

import Command exposing (JoinResponse, joinResponse)
import GameEvent exposing (GameEvent, gameEvent)
import EntityEvent exposing (EntityEvent, entityEvent)
import GoldEvent exposing (GoldEvent, goldEvent)

-- Aggregate Types

type Message =
    JoinResp JoinResponse |
    GameEv GameEvent |
    EntityEv EntityEvent |
    GoldEv GoldEvent

messageInfo : String -> Decoder Message
messageInfo messageType =
    case messageType of

        "command_response" ->
            object1 JoinResp joinResponse

        "game_event" ->
            object1 GameEv gameEvent

        "entity_event" ->
            object1 EntityEv entityEvent

        "gold_event" ->
            object1 GoldEv goldEvent

        _ ->
            fail <| "Unable to parse messageType: " ++ messageType

message : Decoder Message
message =
    at ["type"] string `andThen` messageInfo
