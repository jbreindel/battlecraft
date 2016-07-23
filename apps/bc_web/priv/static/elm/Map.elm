module Map exposing (Effect(..), Msg(..), Model, init, update, view)

import Html exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Effects exposing (Effects)
import Task exposing (Task)
import Keyboard.Extra as Keyboard
import Window
import Http

-- Actions

type Effect =
    PerformCmd (Cmd Msg) |
    NoOp

type Msg =
    MapGetSuccess TmxMap |
    MapGetFail Http.Error |
    KeyboardMsg Keyboard.Model |
    WindowMsg Window.Size

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
    step : Float,
    x : Float,
    y : Float,
    zoom : Float,
    windowHeight : Int,
    windowWidth : Int
}

getMap : Task Http.Error TmxMap
getMap =
    Http.get tmxMap "/static/map.json"

init : Effects Model Effect
init =
    Effects.init {
        map = Nothing,
        step = 50.0,
        x = 0.0,
        y = 0.0,
        zoom = 0.6,
        windowHeight = 800,
        windowWidth = 550
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
                direction = Keyboard.wasdDirection keyboardModel

                updatedModel = updatePos model direction
            in
                Effects.return updatedModel

        WindowMsg windowSize ->
            Effects.return {model |
                            windowHeight = windowSize.height,
                            windowWidth = windowSize.width}

updateX : Model -> Float -> Float
updateX model delta =
    let
        mapWidth = (toFloat 3200) * model.zoom

        windowAdj = mapWidth - (toFloat model.windowWidth)

        maxX = windowAdj / 2

        minX = -1.0 * maxX

        deltaX = model.x + delta
    in
        if deltaX > maxX then
            maxX

        else if deltaX < minX then
            minX

        else
            deltaX

updateY : Model -> Float -> Float
updateY model delta =
    let
        mapHeight = (toFloat 3200) * model.zoom

        windowAdj = mapHeight - (toFloat model.windowHeight)

        maxY = windowAdj / 2

        minY = -1.0 * maxY

        deltaY = model.y + delta
    in
        if deltaY > maxY then
            maxY

        else if deltaY < minY then
            minY

        else
            deltaY

updatePos : Model -> Keyboard.Direction -> Model
updatePos model direction =
    case direction of

        Keyboard.North ->
            let
                stepY = -1 * model.step

                deltaY = updateY model stepY
            in
                {model | y = deltaY}

        Keyboard.NorthEast ->
            let
                stepY = -1 * model.step

                stepX = -1 * model.step

                deltaY = updateY model stepY

                deltaX = updateX model stepX
            in
                {model |
                    x = deltaX,
                    y = deltaY}

        Keyboard.East ->
            let
                stepX = -1 * model.step

                deltaX = updateX model stepX
            in
                {model | x = deltaX}

        Keyboard.SouthEast ->
            let
                stepX = -1 * model.step

                stepY = model.step

                deltaX = updateX model stepX

                deltaY = updateY model stepY
            in
                {model |
                    x = deltaX,
                    y = deltaY}

        Keyboard.South ->
            let
                stepY = model.step

                deltaY = updateY model stepY
            in
                {model | y = deltaY}

        Keyboard.SouthWest ->
            let
                stepX = model.step

                stepY = model.step

                deltaX = updateX model stepX

                deltaY = updateY model stepY
            in
                {model |
                    x = deltaX,
                    y = deltaY}

        Keyboard.West ->
            let
                stepX = model.step

                deltaX = updateX model stepX
            in
                {model | x = deltaX}

        Keyboard.NorthWest ->
            let
                stepX = model.step

                stepY = -1 * model.step

                deltaX = updateX model stepX

                deltaY = updateY model stepY
            in
                {model |
                    x = deltaX,
                    y = deltaY}

        Keyboard.NoDirection ->
            model

-- View

backgroundImageForm : Model -> Collage.Form
backgroundImageForm model =
    Element.fittedImage 3200 3200 "/static/map.png"
        |> Collage.toForm
        |> Collage.scale model.zoom
        |> Collage.move (model.x, model.y)

createMap : Model -> Element
createMap model =
    let
        backgroundForm = backgroundImageForm model
    in
        Collage.collage model.windowWidth model.windowHeight [backgroundForm]

view : Model -> Html Msg
view model =
    case model.map of

        Just map ->
            createMap model |> Element.toHtml

        Nothing ->
            -- TODO maybe loading screen
            div [] []
