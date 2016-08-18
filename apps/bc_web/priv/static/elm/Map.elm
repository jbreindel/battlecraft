
module Map exposing (Effect(..),
                     Msg(..),
                     Model,
                     init,
                     update,
                     view)

import Dict exposing (..)
import Html exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Effects exposing (Effects)
import Task exposing (Task)
import Keyboard.Extra as Keyboard
import Window
import Http

-- Local imports

import TmxMap exposing (TmxMap, tmxMap)
import EntityEvent exposing (EntityEvent, entityEventEntity)
import Entity

-- Actions

type Effect =
    PerformCmd (Cmd Msg) |
    NoOp

type Msg =
    MapGetSuccess TmxMap |
    MapGetFail Http.Error |
    KeyboardMsg Keyboard.Model |
    WindowMsg Window.Size |
    EntityEventMsg EntityEvent |
    EntityMsg Entity.Msg

-- Model

type alias Model = {
    map : Maybe TmxMap,
    step : Float,
    x : Float,
    y : Float,
    zoom : Float,
    windowHeight : Int,
    windowWidth : Int,
    entities : Dict String Entity.Model
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
        windowWidth = 550,
        entities = Dict.empty
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

                updatedModel = updatePos model direction
            in
                Effects.return updatedModel

        WindowMsg windowSize ->
            Effects.return {model |
                            windowHeight = windowSize.height,
                            windowWidth = windowSize.width}

        EntityEventMsg entityEvent ->
            onEntityEvent model entityEvent

        EntityMsg entityMsg ->
            Effects.return model

onEntityEvent : Model -> EntityEvent -> Effects Model Effect
onEntityEvent model entityEvent =
    case model.map of

        Nothing ->
            Effects.return model

        Just map ->
            let
                entity = entityEventEntity entityEvent

                uuid = entity.uuid

                entityMsg = Entity.EntityEv entityEvent

                entityModel = Entity.init map entity

                updatedEntityModel = Entity.update entityMsg entityModel

                updatedEntities = Dict.insert uuid updatedEntityModel model.entities
            in
                Effects.return {model |
                                    entities = updatedEntities}

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

view : Model -> Html Msg
view model =
    case model.map of

        Nothing ->
            Element.empty

        Just map ->
            let
                backgroundForm = backgroundImageForm model

                entityModels = Dict.values model.entities

                entityForms = List.map (\entityModel -> Entity.view map entityModel) entityModels

                entityForm = Collage.group entityForms

                mapForm = Collage.group [backgroundForm, entityForm]
                                |> Collage.scale model.zoom
                                |> Collage.move (model.x, model.y)
            in
                Collage.collage model.windowWidth model.windowHeight [mapForm]
                    |> Element.toHtml
