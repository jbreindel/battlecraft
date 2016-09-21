
module Spawn exposing (Effect(..),
                       Msg(..),
                       Model,
                       init,
                       update,
                       view)

import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class)
import Effects exposing (Effects)
import Keyboard.Extra as Keyboard
import Json.Encode exposing (encode)

-- Local imports

import Command exposing (SpawnCommand, initSpawnCommand, encodeSpawnCommand)

-- Actions

type Effect =
    WsSendMsg String

type Msg =
    KeyboardMsg Keyboard.Model

-- Model

type alias Model = {
    entityType : String,
    gold : Int
}

init : Effects Model Effect
init =
    Effects.return {
        entityType = "",
        gold = 0
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
                            WsSendMsg spawnCmdJson
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

view : Model -> Html Msg
view model =
    div [class "box spawn"] [
        div [class "spawn-content"] [
            h3 [class "spawn-header"] [
                text "Spawn"
            ]
        ]
    ]
