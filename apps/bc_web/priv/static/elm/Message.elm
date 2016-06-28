module Message.Decode exposing (..)

import Json.Decode exposing (..)

-- GameEvent Types

-- Aggregate Types

{--

type Message =
    GameEvent

messageInfo : String -> Decoder Message
messageInfo messageType =
    case messageType of
        "game_event" ->
            gameEvent

message : Decoder Message
message =
    ("type" := string) `andThen` messageInfo

--}
