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

type alias JoinErrorResponse = {
    responseType : String,
    error : String
}

joinErrorResponse : Json.Decode.Decoder JoinErrorResponse
joinErrorResponse =
    at ["command_response"] <| object2 JoinErrorResponse
        ("response_type" := Json.Decode.string)
        ("error" := Json.Decode.string)

type alias JoinSuccessResponse = {
    responseType : String,
    playerId : Int
}

joinSuccessResponse : Json.Decode.Decoder JoinSuccessResponse
joinSuccessResponse =
    at ["command_response"] <| object2 JoinSuccessResponse
        ("response_type" := Json.Decode.string)
        ("player_id" := Json.Decode.int)

-- Aggregate types

type JoinResponse =
    JoinErr JoinErrorResponse |
    JoinSucc JoinSuccessResponse

joinResponseInfo : String -> Decoder JoinResponse
joinResponseInfo responseType =
    case responseType of
        "join_response" ->
            object1 JoinSucc joinSuccessResponse
        "join_error" ->
            object1 JoinErr joinErrorResponse
        _ ->
            object1 JoinErr joinErrorResponse

joinResponse : Decoder JoinResponse
joinResponse =
    at ["command_response", "response_type"] Json.Decode.string
        `andThen` joinResponseInfo
