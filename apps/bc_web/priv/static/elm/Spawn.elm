
module Spawn exposing (Effect(..),
                       Msg(..),
                       Model,
                       init,
                       update,
                       view)

import Html exposing (Html, div, h5, p, figure, a, img, text)
import Html.Attributes exposing (class, src)
import Html.Lazy exposing (lazy)
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

    else if Keyboard.isPressed Keyboard.CharD keyboardModel then
        Just "roman_guard"

    else if Keyboard.isPressed Keyboard.CharW keyboardModel then
        Just "chaos_rider"

    else
        Nothing

-- View

view : Model -> Html Msg
view model =
    lazy spawnTile model

spawnTile : Model -> Html Msg
spawnTile model =
    div [class "tile is-vertical is-2 box is-overlay spawn-content"] [

        -- Heading
        div [class "columns"] [
            spawnTitleColumn,
            goldColumn model
        ],

        -- Entities
        div [class "columns"] [
            championColumn,
            demonColumn
        ],

        div [class "columns"] [
            romanGuardColumn,
            chaosRiderColumn
        ]
    ]

spawnTitleColumn : Html Msg
spawnTitleColumn =
    div [class "column is-two-thirds is-flex is-vcentered"] [
        h5 [class "title is-5"] [
            text "Spawn"
        ]
    ]

goldColumn : Model -> Html Msg
goldColumn model =
    div [class "column is-flex is-vcentered"] [
        img [src "/static/assets/interface/upgrades/building.PNG"] [],
        p [class "gold-amount"] [
            text (toString model.gold)
        ]
    ]


championColumn : Html Msg
championColumn =
    div [class "column is-half is-flex is-vcentered"] [
        div [class "columns is-gapless"] [
            div [class "column is-flex is-vcentered"] [
                a [class "button is-medium"] [
                    h5 [class "title is-5 entity-key"] [
                        text "A"
                    ]
                ]
            ],
            div [class "column is-flex is-vcentered"] [
                div [class "entity-info"] [
                    div [class "columns is-gapless is-marginless"] [
                        figure [class "image is-48x48 entity-image"] [
                            img [class "entity", src "/static/assets/units/b_champion/champion_stand_1.PNG"] []
                        ]
                    ],
                    div [class "columns is-gapless is-marginless"] [
                        p [class "is-margin-auto"] [text "100"]
                    ]
                ]
            ]
        ]
    ]

demonColumn : Html Msg
demonColumn =
    div [class "column is-half is-flex is-vcentered"] [
        div [class "columns is-gapless"] [
            div [class "column is-flex is-vcentered"] [
                a [class "button is-medium"] [
                    h5 [class "title is-5 entity-key"] [
                        text "S"
                    ]
                ]
            ],
            div [class "column is-flex is-vcentered"] [
                div [class "entity-info"] [
                    div [class "columns is-gapless is-marginless"] [
                        figure [class "image is-48x48 entity-image"] [
                            img [class "entity", src "/static/assets/units/b_demon/demon_stand_1.PNG"] []
                        ]
                    ],
                    div [class "columns is-gapless is-marginless"] [
                        p [class "is-margin-auto"] [text "300"]
                    ]
                ]
            ]
        ]
    ]

romanGuardColumn : Html Msg
romanGuardColumn =
    div [class "column is-half is-flex is-vcentered"] [
        div [class "columns is-gapless"] [
            div [class "column is-flex is-vcentered"] [
                a [class "button is-medium"] [
                    h5 [class "title is-5 entity-key"] [
                        text "D"
                    ]
                ]
            ],
            div [class "column is-flex is-vcentered"] [
                div [class "entity-info"] [
                    div [class "columns is-gapless is-marginless"] [
                        figure [class "image is-48x48 entity-image"] [
                            img [class "entity", src "/static/assets/units/r_roman_guard/stand.PNG"] []
                        ]
                    ],
                    div [class "columns is-gapless is-marginless"] [
                        p [class "is-margin-auto"] [text "500"]
                    ]
                ]
            ]
        ]
    ]

chaosRiderColumn : Html Msg
chaosRiderColumn =
    div [class "column is-half is-flex is-vcentered"] [
        div [class "columns is-gapless"] [
            div [class "column is-flex is-vcentered"] [
                a [class "button is-medium"] [
                    h5 [class "title is-5 entity-key"] [
                        text "W"
                    ]
                ]
            ],
            div [class "column is-flex is-vcentered"] [
                div [class "entity-info"] [
                    div [class "columns is-gapless is-marginless"] [
                        figure [class "image is-48x48 entity-image"] [
                            img [class "entity", src "/static/assets/units/b_chaos_rider/chaosreiter_walk_2.gif"] []
                        ]
                    ],
                    div [class "columns is-gapless is-marginless"] [
                        p [class "is-margin-auto"] [text "1000"]
                    ]
                ]
            ]
        ]
    ]
