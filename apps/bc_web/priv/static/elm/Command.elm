module Command exposing (..)

import Json.Decode exposing (..)
import Json.Encode exposing (..)

-- Command types

type alias JoinCommand = {
    commandType : String,
    handle : String
}

initJoinCommand : String -> JoinCommand
initJoinCommand handle =
    (JoinCommand "join_command" handle)

encodeJoinCommand : JoinCommand -> Json.Encode.Value
encodeJoinCommand joinCmd =
    object [
        ("command_type", Json.Encode.string joinCmd.commandType),
        ("handle", Json.Encode.string joinCmd.handle)
    ]

-- Response types

type alias ResponseError = {
    responseType : String,
    error : String
}

responseError : Json.Decode.Decoder ResponseError
responseError =
    object2 ResponseError
        ("response_type" := Json.Decode.string)
        ("reason" := Json.Decode.string)

type alias JoinResponse = {
    responseType : String,
    playerId : Int
}

joinResponse : Json.Decode.Decoder JoinResponse
joinResponse =
    object2 JoinResponse
        ("response_type" := Json.Decode.string)
        ("player_id" := Json.Decode.int)

-- Aggregate types

type Response =
    ResponseErr ResponseError |
    JoinResp JoinResponse
