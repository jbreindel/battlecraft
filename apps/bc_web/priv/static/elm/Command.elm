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

type alias ErrorResponse = {
    responseType : String,
    error : String
}

errorResponse : Json.Decode.Decoder ResponseError
errorResponse =
    object2 ErrorResponse
        ("response_type" := Json.Decode.string)
        ("error" := Json.Decode.string)

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
    ErrorResp ErrorResponse |
    JoinResp JoinResponse

responseInfo : String -> Decoder Response
responseInfo responseType =
    case responseType of
        "join_response" ->
            object1 JoinResp joinResponse
        "join_error" ->
            object1 JoinError errorResponse
        _ ->
            object1 JoinError errorResponse

response : Decoder Response
response =
    at ["command_response", "response_type"] string `andThen` responseInfo
