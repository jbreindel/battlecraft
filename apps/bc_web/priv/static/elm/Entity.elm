
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
import Task exposing (Task)
import Process
import Element
import Collage

-- Local imports

import TmxMap exposing (TmxMap, tmxMapHeight, tmxMapWidth)
import EntityEvent exposing (Vertex, Entity, EntityEvent,
                             EntitySpawnedEvent, EntityMovedEvent)

-- Actions

type Effect =
    PerformCmd (Cmd Msg)

type Msg =
    ReceiveEntityEv EntityEvent |
    ConsumeEntityEv EntityEvent |
    MoveTick String Float |
    MoveSuccess |
    MoveFail |
    NoOp

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
    movePosition : (Float, Float),
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
        movePosition = (0.0, 0.0),
        orientation = Down,
        entityState = Standing,
        eventBuffer = Deque.empty
    }

-- Update

update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of

        ReceiveEntityEv entityEvent ->
            onReceiveEntityEvent entityEvent model

        MoveTick _ delta ->
            onMoveTick delta model

        MoveSuccess ->
            onMoveComplete model

        MoveFail ->
            onMoveComplete model

        NoOp ->
            Effects.return model

onMoveComplete : Model -> Effects Model Effect
onMoveComplete model =
    let
        (_, eventBuffer) =
            Deque.popFront model.eventBuffer

        (mbEntityEvent, updatedEventBuffer) =
            Deque.popFront eventBuffer
    in
        case mbEntityEvent of

            Just entityEvent ->
                let
                    cmd = Task.succeed entityEvent
                            |> Task.perform ConsumeEntityEv NoOp
                in
                    Effects.init {model |
                        eventBuffer = updatedEventBuffer
                    } [PerformCmd cmd]

            Nothing ->
                Effects.return {model |
                                    eventBuffer = updatedEventBuffer}

onMoveTick : Float -> Model -> Effects Model Effect
onMoveTick delta model =
    let
        (x, y) = model.position

        (moveX, moveY) = model.movePosition
    in
        if x == moveX && moveY > y then
            Effects.return {model | position = (x, y + delta)}

        else if x == moveX && moveY < y then
            Effects.return {model | position = (x, y - delta)}

        else if y == moveY && moveX > x then
            Effects.return {model | position = (x + delta, y)}

        else if y == moveY && moveX < x then
            Effects.return {model | position = (x - delta, y)}

        else
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
                |> abs

        deltaY = y1 - y2
                |> abs
    in
        if deltaX > 0.0 then
            deltaX

        else
            deltaY

moveTime : String -> Float
moveTime entityType =
    case entityType of

        "champion" ->
            0.6

        "demon" ->
            0.5

        _ ->
            0.5

onEntityMovedEvent : EntityMovedEvent -> Model -> Effects Model Effect
onEntityMovedEvent entityMovedEvent model =
    let
        entityEvent = EntityEvent.EntityMovedEv entityMovedEvent

        updatedEventBuffer =
            Deque.pushBack entityEvent model.eventBuffer

        movePosition = entityMovedEvent.entity.vertices
                        |> vertexMatrix
                        |> entityPosition model.tmxMap

        posDelta = deltaPos model.position movePosition

        moveDelta = 2

        tick = posDelta / moveDelta

        moveSpeed = moveTime model.entity.entityType

        time = 1000.0 - (1000.0 * moveSpeed)

        tickTime = time / tick

        cmd = List.repeat tick moveDelta
                |> List.map (
                    \delta ->
                        Task.succeed (MoveTick model.entity.uuid delta)
                            `Task.andThen` (
                                \msg -> Process.sleep tickTime
                            )
                )
                |> List.foldl Task.andThen (Task.succeed NoOp)
                |> Task.perform MoveSuccess MoveFail
    in
        Effects.init {model |
            movePosition = movePosition,
            eventBuffer = updatedEventBuffer
        } [PerformCmd cmd]

onReceiveEntityEvent : EntityEvent -> Model -> Effects Model Effect
onReceiveEntityEvent entityEvent model =
    if Deque.isEmpty model.eventBuffer then
        onConsumeEntityEvent entityEvent model

    else
        let
            updatedEventBuffer =
                Deque.pushBack entityEvent model.eventBuffer
        in
            Effects.return {model |
                                eventBuffer = updatedEventBuffer}

onConsumeEntityEvent : EntityEvent -> Model -> Effects Model Effect
onConsumeEntityEvent entityEvent model =
    case entityEvent of

        EntityEvent.EntitySpawnedEv entitySpawnedEvent ->
            onEntitySpawnedEvent entitySpawnedEvent model

        EntityEvent.EntityMovedEv entityMovedEvent ->
            onEntityMovedEvent entityMovedEvent model

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
