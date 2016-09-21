
module Map exposing (Effect(..),
                     Msg(..),
                     Model,
                     init,
                     update,
                     subscriptions,
                     view)

import Dict exposing (..)
import Html exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Effects exposing (Effects)
import Task exposing (Task)
import Keyboard.Extra as Keyboard
import Time exposing (Time)
import AnimationFrame
import Window
import Http

-- Local imports

import TmxMap exposing (MinTmxMap, minTmxMap)
import EntityEvent exposing (EntityEvent, entityEventEntity)
import Entity

-- Actions

type Effect =
    PerformCmd (Cmd Msg)

type Msg =
    MapGetSuccess MinTmxMap |
    MapGetFail Http.Error |
    KeyboardMsg Keyboard.Model |
    WindowMsg Window.Size |
    EntityEventMsg EntityEvent |
    EntityMsg Entity.Msg |
    OnAnimationFrame Time

-- Model

type alias Model = {
    map : Maybe MinTmxMap,
    step : Float,
    x : Float,
    y : Float,
    zoom : Float,
    windowHeight : Int,
    windowWidth : Int,
    entities : Dict String (Entity.Model)
}

getMap : Task Http.Error MinTmxMap
getMap =
    Http.get minTmxMap "/static/map.json"

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
            onEntityMsg model entityMsg

        OnAnimationFrame time ->
            onAnimationFrame time model

onEntityEvent : Model -> EntityEvent -> Effects Model Effect
onEntityEvent model entityEvent =
    case model.map of

        Nothing ->
            Effects.return model

        Just tmxMap ->
            let
                entity = entityEventEntity entityEvent

                uuid = entity.uuid

                entityMsg = Entity.ReceiveEntityEv entityEvent

                (entityModel, initEntityEffects) =
                    case Dict.get entity.uuid model.entities of

                        Just model ->
                            (model, [])

                        Nothing ->
                            Entity.init tmxMap entity

                (updatedEntityModel, updatedEntityEffects) =
                    Entity.update entityMsg entityModel

                entityEffects = initEntityEffects ++ updatedEntityEffects

                updatedEntities =
                    Dict.insert uuid updatedEntityModel model.entities
            in
                Effects.return {model |
                    entities = updatedEntities
                } `Effects.andThen`
                    Effects.handle handleEntityEffect entityEffects

onEntityMsg : Model -> Entity.Msg -> Effects Model Effect
onEntityMsg model entityMsg =
    let
        mbEntity =
            case entityMsg of

                Entity.ReceiveEntityEv entityEvent ->
                    entityEventEntity entityEvent
                        |> Maybe.Just

                Entity.ConsumeEntityEv entity ->
                    Maybe.Just entity

                _ -> Maybe.Nothing

        in
            mbEntity `Maybe.andThen` (
                \entity ->
                    Dict.get entity.uuid model.entities `Maybe.andThen` (
                        \entityModel ->
                            let
                                updatedEntityEffects =
                                    Entity.update entityMsg entityModel
                            in
                                Maybe.Just (entity.uuid, updatedEntityEffects)

                )
            ) `Maybe.andThen` (
                \(uuid, updatedEntityEffects) ->
                    let
                        (entityModel, entityEffects) =
                            updatedEntityEffects

                        updatedEntities =
                            Dict.insert uuid entityModel model.entities
                    in
                        Effects.return {model |
                            entities = updatedEntities
                        } `Effects.andThen`
                            Effects.handle handleEntityEffect entityEffects
                        |> Maybe.Just

            ) |> Maybe.withDefault (Effects.return model)

onAnimationFrame : Time -> Model -> Effects Model Effect
onAnimationFrame time model =
    let
        entityCmdMsg = Entity.OnAnimationFrame time

        (entityTuples, entityEffects) =
            Dict.toList model.entities
                |> List.map (
                    \(uuid, entityModel) ->
                        let
                            (updatedModel, effects) =
                                Entity.update entityCmdMsg entityModel
                        in
                            ((uuid, updatedModel), effects)
                )
                |> Effects.batch

        updatedEntities = Dict.fromList entityTuples
    in
        Effects.return {model |
            entities = updatedEntities
        } `Effects.andThen` Effects.handle handleEntityEffect entityEffects

handleEntityEffect : Effects.Handler Entity.Effect Model Effect
handleEntityEffect entityEffect model =
    case entityEffect of

        Entity.PerformCmd entityCmdMsg ->
            let
                mapEffect =
                    Cmd.map EntityMsg entityCmdMsg
                        |> PerformCmd
            in
                Effects.init model [mapEffect]

        Entity.EntityDied entity ->
            let
                entities =
                    Dict.remove entity.uuid model.entities
            in
                Effects.return {model |
                                    entities = entities}

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

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        annimationFrameSub =
            if Dict.isEmpty model.entities then
                Sub.none

            else
                -- TODO actually call Entity.subscriptions
                AnimationFrame.times OnAnimationFrame

        windowResizeSub = Window.resizes WindowMsg
    in
        Sub.batch [
            annimationFrameSub,
            windowResizeSub
        ]

-- View

backgroundImageForm : Collage.Form
backgroundImageForm =
    Element.fittedImage 3200 3200 "/static/map.png"
        |> Collage.toForm

view : Model -> Html Msg
view model =
    case model.map of

        Nothing ->
            Element.empty
                |> Element.toHtml

        Just tmxMap ->
            let
                backgroundForm = backgroundImageForm

                entityForms =
                    Dict.values model.entities
                    |> List.concatMap (
                        \entityModel ->
                            Entity.view entityModel
                    )

                entityForm = Collage.group entityForms
                                |> Collage.scale model.zoom
                                |> Collage.move (model.x, model.y)

                mapForm = backgroundForm
                                |> Collage.scale model.zoom
                                |> Collage.move (model.x, model.y)
            in
                Collage.collage model.windowWidth model.windowHeight
                    [mapForm, entityForm]
                    |> Element.toHtml
