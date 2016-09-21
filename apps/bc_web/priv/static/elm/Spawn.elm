
module Spawn exposing (Effect(..),
                       Msg(..),
                       Model,
                       init,
                       update,
                       view)

import Html exposing (Html, div, h5, p, img, text)
import Html.Attributes exposing (class, src)
import Effects exposing (Effects)
import Keyboard.Extra as Keyboard
import Json.Encode exposing (encode)

-- Local imports

import Command exposing (SpawnCommand, initSpawnCommand, encodeSpawnCommand)
import GoldEvent exposing (GoldEvent, goldEventGold)

-- Actions

type Effect =
    WsSendMsg String

type Msg =
    KeyboardMsg Keyboard.Model |
    GoldEv GoldEvent

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
            onKeyboardMsg keyboardModel model

        GoldEv goldEvent ->
            onGoldEvent goldEvent model

onKeyboardMsg : Keyboard.Model -> Model -> Effects Model Effect
onKeyboardMsg keyboardModel model =
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

onGoldEvent : GoldEvent -> Model -> Effects Model Effect
onGoldEvent goldEvent model =
    let
        gold = goldEventGold goldEvent
    in
        Effects.return {model | gold = gold}

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
    div [class "tile is-vertical is-2 box is-overlay spawn-content"] [
        div [class "columns"] [
            div [class "column is-two-thirds is-flex spawn-header"] [
                h5 [class "title is-5"] [
                    text "Spawn"
                ]
            ],
            div [class "column is-flex spawn-gold"] [
                img [src "/static/assets/interface/upgrades/building.PNG"] [],
                p [class "gold-amount"] [
                    text (toString model.gold)
                ]
            ]
        ]
    ]
