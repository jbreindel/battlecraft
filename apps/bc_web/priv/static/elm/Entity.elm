
module Entity exposing (Effect(..),
                        Msg(..),
                        Model,
                        init,
                        update,
                        view)

import Color exposing (green, orange, red)
import Dict exposing (Dict)
import Deque exposing (Deque)
import Effects exposing (Effects)
import Element
import Collage

-- Local imports

import TmxMap exposing (TmxMap, tmxMapHeight, tmxMapWidth)
import EntityEvent exposing (Vertex, Entity, EntityEvent)

-- Actions

type Effect =
    PerformCmd (Cmd Msg)

type Msg =
    EntityEv EntityEvent |
    EventTick

-- Model

type Orientation =
    Up |
    Right |
    Down |
    Left

type EntityState =
    Standing |
    Moving |
    Attacking

type alias Model = {
    entity : Entity,
    tmxMap : TmxMap,
    matrix : Dict Int (List Int),
    position : (Float, Float),
    movementPosition : (Float, Float),
    orientation : Orientation,
    entityState : EntityState,
    eventBuffer : Deque EntityEvent
}

init : TmxMap -> Entity -> Effects Model Effect
init tmxMap entity =
    Effects.return {
        entity = entity,
        tmxMap = tmxMap,
        matrix = Dict.empty,
        position = (0.0, 0.0),
        movementPosition = (0.0, 0.0),
        orientation = Down,
        entityState = Standing,
        eventBuffer = Deque.empty
    }

-- Update

update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of

        EntityEv entityEvent ->
            onEntityEvent entityEvent model

        NoOp ->
            Effects.return model

onEntitySpawnedEvent : EntitySpawnedEvent -> Model -> Effects Model Effect
onEntitySpawnedEvent entitySpawnedEvent model =
    let
        matrix = vertexMatrix entitySpawnedEvent.entity.vertices

        position = entityPosition model.tmxMap matrix
    in
        Effects.return {model |
                            matrix = matrix,
                            position = position}

deltaPos : (Float, Float) -> (Float, Float) -> Float
deltaPos (x1, y1) (x2, y2) =
    let
        deltaX = x1 - x2

        deltaY = y1 - y2
    in
        if deltaX > 0.0 then
            deltaX
        else
            deltaY

onEntityMovedEvent : EntityMovedEvent -> Model -> Effects Model Effect
onEntityMovedEvent entityMovedEvent model =
    let
        updatedEventBuffer = Deque.pushBack entityMovedEvent model.eventBuffer
    in
        if Deque.isEmpty model.eventBuffer then
            let
                movementPosition = entityMovedEvent.entity.vertices
                                    |> vertexMatrix
                                    |> entityPosition model.tmxMap
            in
                Effects.init {model |
                                eventBuffer = updatedEventBuffer,
                                movementPosition = movementPosition} [

                ]

onEntityEvent : EntityEvent -> Model -> Effects Model Effect
onEntityEvent entityEvent model =
    case entityEvent of

        EntityEvent.EntitySpawnedEv entitySpawnedEvent ->
            onEntitySpawnedEvent entitySpawnedEvent model

        EntityEvent.EntityMovedEv entityMovedEvent ->
            let

                updatedEffectBuffer =
            in
                Effects.return {model |
                                    movementPosition = movementPosition}

        EntityEvent.EntityDamagedEv entityDamagedEvent ->
            Effects.return {model |
                                entity = entityDamagedEvent.entity}

vertexMatrix : List Vertex -> Dict Int (List Int)
vertexMatrix vertices =
    List.foldl (
            \vertex matrix ->
                let
                    row = vertex.row

                    cols = Dict.get row matrix
                            |> Maybe.withDefault []

                    updatedCols = vertex.col :: cols
                in
                    Dict.insert row updatedCols matrix
        )  Dict.empty vertices

entityRowCount : Dict Int (List Int) -> Int
entityRowCount matrix =
    let
        rows = Dict.keys matrix
    in
        (List.length rows)

entityHeight : TmxMap -> Dict Int (List Int) -> Int
entityHeight tmxMap matrix =
    let
        entityRows = entityRowCount matrix
    in
        (entityRows * tmxMap.tileHeight)

entityColCount : Dict Int (List Int) -> Int
entityColCount matrix =
    let
        row = Dict.keys matrix
                |> List.head
                |> Maybe.withDefault -1

        cols = Dict.get row matrix
                |> Maybe.withDefault []
    in
        (List.length cols)

entityWidth : TmxMap -> Dict Int (List Int) -> Int
entityWidth tmxMap matrix =
    let
        colCount = entityColCount matrix
    in
        (colCount * tmxMap.tileWidth)

entityPosition : TmxMap -> Dict Int (List Int) -> (Float, Float)
entityPosition tmxMap matrix =
    let
        -- Y pos
        minRow = Dict.keys matrix
                    |> List.minimum
                    |> Maybe.withDefault -1

        height = entityHeight tmxMap matrix
                    |> toFloat

        heightOffset = height / 2

        tileHeight = tmxMap.tileHeight
                    |> toFloat

        y = ((toFloat minRow) * tileHeight) + heightOffset

        mapHeight = tmxMapHeight tmxMap
                        |> toFloat

        offsetY = (mapHeight / 2) - y

        -- X pos
        minCol = Dict.get minRow matrix
                    |> Maybe.withDefault []
                    |> List.minimum
                    |> Maybe.withDefault -1

        width = entityWidth tmxMap matrix
                    |> toFloat

        widthOffset = width / 2

        tileWidth = tmxMap.tileWidth
                |> toFloat

        x = ((toFloat minCol) * tileWidth) + widthOffset

        mapWidth = tmxMapWidth tmxMap
                        |> toFloat

        offsetX = x - (mapWidth / 2)
    in
        (offsetX, offsetY)

-- View

entityAngle : Orientation -> Float
entityAngle orientation =
    case orientation of

        Up ->
            degrees 0

        Right ->
            degrees 270

        Down ->
            degrees 180

        Left ->
            degrees 90

championImage : Model -> Element.Element
championImage model =
    case model.entityState of

        Standing ->
            Element.image 64 64
                "/static/assets/units/b_champion/champion_stand_1.PNG"

        Moving ->
            Element.image 64 64
                "/static/assets/gifs/champion2.gif"

        Attacking ->
            Element.image 64 64
                "/static/assets/gifs/champion.gif"

entityTypeImage : Model -> Element.Element
entityTypeImage model =
    case model.entity.entityType of

        "base" ->
            Element.empty

        "champion" ->
            championImage model

        _ ->
            Element.empty

entityImage : Model -> Collage.Form
entityImage model =
    let
        imageAngle = entityAngle model.orientation
    in
        entityTypeImage model
            |> Collage.toForm
            |> Collage.rotate imageAngle

entityHealthBar : Model -> Collage.Form
entityHealthBar model =
    let
        width = entityWidth model.tmxMap model.matrix
                    |> toFloat

        offsetWidth = width - (width * 0.25)

        health = model.entity.health
                    |> toFloat

        maxHealth = model.entity.maxHealth
                        |> toFloat

        healthPct = health / maxHealth
    in
        Collage.rect offsetWidth 2.0
            |> Collage.filled green

view : Model -> Collage.Form
view model =
    let
        (x, y) = model.position

        backgroundForm = entityImage model

        healthBarForm = entityHealthBar model

        entityForm = Collage.group [backgroundForm, healthBarForm]
    in
       Collage.move (x, y) entityForm
