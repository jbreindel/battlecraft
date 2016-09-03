
module Spawn exposing (Effect(..),
                       Msg(..),
                       Model,
                       init,
                       update)

import Effects exposing (Effects)
import Keyboard.Extra as Keyboard
import Json.Encode exposing (encode)

import Command exposing (SpawnCommand, initSpawnCommand, encodeSpawnCommand)

-- Actions

type Effect =
    WsSendMessage String

type Msg =
    KeyboardMsg Keyboard.Model

-- Model

type alias Model = {
    entityType : String
}

init : Effects Model Effect
init =
    Effects.return {
        entityType = ""
    }

-- Update

update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of

        KeyboardMsg keyboardModel ->
            case entityType keyboardModel of
                Just entity ->
                    let
                        spawnCmdJson = initSpawnCommand entity
                                        |> encodeSpawnCommand
                                        |> encode 0
                    in
                        Effects.init {model | entityType = entity} [
                            WsSendMessage spawnCmdJson
                        ]
                Nothing ->
                    Effects.return model

entityType : Keyboard.Model -> Maybe String
entityType keyboardModel =
    if Keyboard.isPressed Keyboard.CharA keyboardModel then
        Just "champion"

    else if Keyboard.isPressed Keyboard.CharS keyboardModel then
        Just "demon"

    else
        Nothing
