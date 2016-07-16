module Map exposing (Effect(..), Msg(..), Model, init, update, view)

import Html exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Effects exposing (Effects)
import Task exposing (Task)
import Keyboard.Extra as Keyboard
import Http

-- Actions

type Effect =
    PerformCmd (Cmd Msg) |
    NoOp

type Msg =
    MapGetSuccess TmxMap |
    MapGetFail Http.Error |
    KeyboardMsg Keyboard.Model

-- Model

type alias TmxTileLayer = {
    name : String,
    data : List Int,
    height : Int,
    width : Int,
    visible : Bool,
    x : Int,
    y : Int,
    opacity : Int
}

tmxTileLayer : Decoder TmxTileLayer
tmxTileLayer =
    succeed TmxTileLayer
        |: ("name" := string)
        |: ("data" := list int)
        |: ("height" := int)
        |: ("width" := int)
        |: ("visible" := bool)
        |: ("x" := int)
        |: ("y" := int)
        |: ("opacity" := int)

type alias TmxObject = {
    id : Int,
    height : Int,
    width : Int,
    rotation : Int,
    visible : Bool,
    x : Int,
    y : Int
}

tmxObject : Decoder TmxObject
tmxObject =
    succeed TmxObject
        |: ("id" := int)
        |: ("height" := int)
        |: ("width" := int)
        |: ("rotation" := int)
        |: ("visible" := bool)
        |: ("x" := int)
        |: ("y" := int)

type alias TmxObjectLayer = {
    name : String,
    objects : List TmxObject,
    height : Int,
    width : Int,
    opacity : Int,
    x : Int,
    y : Int
}

tmxObjectLayer : Decoder TmxObjectLayer
tmxObjectLayer =
    succeed TmxObjectLayer
        |: ("name" := string)
        |: ("objects" := list tmxObject)
        |: ("height" := int)
        |: ("width" := int)
        |: ("opacity" := int)
        |: ("x" := int)
        |: ("y" := int)

type TmxLayer =
    TmxTlLayer TmxTileLayer |
    TmxObjLayer TmxObjectLayer

tmxLayerInfo : String -> Decoder TmxLayer
tmxLayerInfo layerType =
    case layerType of

        "tilelayer" ->
            object1 TmxTlLayer tmxTileLayer

        "objectgroup" ->
            object1 TmxObjLayer tmxObjectLayer

        lyrType ->
            Debug.crash lyrType

tmxLayer : Decoder TmxLayer
tmxLayer =
    at ["type"] string `andThen` tmxLayerInfo

type alias TmxTileSet = {
    name : String,
    firstGid : Int,
    image : String,
    imageHeight : Int,
    imageWidth : Int,
    columns : Int,
    margin : Int,
    spacing : Int,
    tileCount : Int,
    tileHeight : Int,
    tileWidth : Int
}

tmxTileSet : Decoder TmxTileSet
tmxTileSet =
    succeed TmxTileSet
        |: ("name" := string)
        |: ("firstgid" := int)
        |: ("image" := string)
        |: ("imageheight" := int)
        |: ("imagewidth" := int)
        |: ("columns" := int)
        |: ("margin" := int)
        |: ("spacing" := int)
        |: ("tilecount" := int)
        |: ("tileheight" := int)
        |: ("tilewidth" := int)

type alias TmxMap = {
    height : Int,
    width : Int,
    tileHeight : Int,
    tileWidth : Int,
    layers : List TmxLayer,
    tileSets : List TmxTileSet
}

tmxMap : Decoder TmxMap
tmxMap =
    succeed TmxMap
        |: ("height" := int)
        |: ("width" := int)
        |: ("tileheight" := int)
        |: ("tilewidth" := int)
        |: ("layers" := list tmxLayer)
        |: ("tilesets" := list tmxTileSet)

type alias Model = {
    map : Maybe TmxMap,
    step : Int,
    x : Int,
    y : Int,
    zoom : Float
}

getMap : Task Http.Error TmxMap
getMap =
    Http.get tmxMap "/static/map.json"

init : Effects Model Effect
init =
    Effects.init {
        map = Nothing,
        step = 100,
        x = 0,
        y = 0,
        zoom = 0.65
    } [PerformCmd <| Task.perform MapGetFail MapGetSuccess getMap]

-- Update

update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of

        MapGetSuccess tmxMap ->
            Effects.return {model | map = Just tmxMap}

        MapGetFail httpError ->
            Debug.crash "Http request failed!"

        KeyboardMsg keyboardModel ->
            let
                direction = Keyboard.arrowsDirection keyboardModel
            in
                Effects.return <| updatePos model direction

updatePos : Model -> Keyboard.Direction -> Model
updatePos model direction =
    case direction of

        Keyboard.North ->
            {model | y = model.y + model.step}

        Keyboard.NorthEast ->
            {model |
                x = model.x + model.step,
                y = model.y + model.step}

        Keyboard.East ->
            {model | x = model.x + model.step}

        Keyboard.SouthEast ->
            {model |
                x = model.x + model.step,
                y = model.y - model.step}

        Keyboard.South ->
            {model | y = model.y - model.step}

        Keyboard.SouthWest ->
            {model |
                x = model.x - model.step,
                y = model.y - model.step}

        Keyboard.West ->
            {model | x = model.x - model.step}

        Keyboard.NorthWest ->
            {model |
                x = model.x - model.step,
                y = model.y + model.step}

        Keyboard.NoDirection ->
            model

-- View

backgroundImageForm : Model -> Collage.Form
backgroundImageForm model =
    Element.fittedImage 3200 3200 "/static/map.png"
        |> Collage.toForm
        |> Collage.scale model.zoom
        |> Collage.move (toFloat model.x, toFloat model.y)

createMap : Model -> Element
createMap model =
    let
        backgroundForm = backgroundImageForm model
    in
        Collage.collage 960 700 [backgroundForm]

view : Model -> Html Msg
view model =
    case model.map of

        Just map ->
            createMap model |> Element.toHtml

        Nothing ->
            -- TODO maybe loading screen
            div [] []
