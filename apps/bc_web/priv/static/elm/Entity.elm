
module Entity exposing (Effect(..),
                        Msg(..),
                        Model,
                        init,
                        update,
                        subscriptions,
                        view)

import Color exposing (green, yellow, red)
import Dict exposing (Dict)
import Maybe exposing (Maybe)
import Deque exposing (Deque)
import Effects exposing (Effects)
import Task exposing (Task)
import Time exposing (Time)
import Animation exposing (Animation)
import AnimationFrame
import Ease
import Element
import Collage

-- Local imports

import TmxMap exposing (MinTmxMap, tmxMapHeight, tmxMapWidth)
import EntityEvent exposing (Vertex, Entity, EntityEvent,
                             EntitySpawnedEvent, EntityMovedEvent)

-- Actions

type Effect =
    PerformCmd (Cmd Msg) |
    EntityDied Entity

type Msg =
    ReceiveEntityEv EntityEvent |
    ConsumeEntityEv Entity |
    OnAnimationFrame Time |
    NoOp String

-- Model

type EntityState =
    Standing |
    Moving |
    Attacking |
    Dead

type alias Model = {
    entity : Entity,
    tmxMap : MinTmxMap,
    matrix : Dict Int (List Int),
    position : (Float, Float),
    animation : Maybe (String, Animation),
    entityState : EntityState,
    eventBuffer : Deque EntityEvent,
    clock : Time
}

init : MinTmxMap -> Entity -> Effects Model Effect
init tmxMap entity =
    Effects.return {
        entity = entity,
        tmxMap = tmxMap,
        matrix = Dict.empty,
        position = (0.0, 0.0),
        animation = Nothing,
        entityState = Standing,
        eventBuffer = Deque.empty,
        clock = 0.0
    }

-- Update

update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of

        ReceiveEntityEv entityEvent ->
            onReceiveEntityEvent entityEvent model

        ConsumeEntityEv entity ->
            onConsumeEntityEvent model

        OnAnimationFrame time ->
            onAnimationFrame time model

        NoOp reason ->
            Effects.return model

onReceiveEntityEvent : EntityEvent -> Model -> Effects Model Effect
onReceiveEntityEvent entityEvent model =
    let
        updatedEventBuffer =
            Deque.pushBack entityEvent model.eventBuffer
    in
        queueEntityEvent {model |
                            eventBuffer = updatedEventBuffer}

onConsumeEntityEvent : Model -> Effects Model Effect
onConsumeEntityEvent model =
    Deque.first model.eventBuffer `Maybe.andThen` (
        \entityEvent ->
            case entityEvent of

                EntityEvent.EntitySpawnedEv entitySpawnedEvent ->
                    onEntitySpawnedEvent entitySpawnedEvent model
                        |> Maybe.Just

                EntityEvent.EntityMovedEv entityMovedEvent ->
                    onEntityMovedEvent entityMovedEvent model
                        |> Maybe.Just

                EntityEvent.EntityDamagedEv entityDamagedEvent ->
                    popAndQueueEntityEvent {model |
                                        entity = entityDamagedEvent.entity}
                        |> Maybe.Just

                EntityEvent.EntityAttackingEv entityAttackingEvent ->
                    popAndQueueEntityEvent {model |
                                        entity = entityAttackingEvent.entity,
                                        entityState = Attacking}
                        |> Maybe.Just

                EntityEvent.EntityDiedEv entityDiedEvent ->
                    -- TODO annimate blood pool
                    Effects.init {model |
                                    entity = entityDiedEvent.entity,
                                    entityState = Dead} [
                        EntityDied entityDiedEvent.entity
                    ] |> Maybe.Just

    ) |> Maybe.withDefault (Effects.return model)

onAnimationFrame : Time -> Model -> Effects Model Effect
onAnimationFrame time model =
    let
        (x, y) = model.position
    in
        case model.animation of

            Just ("x", animation) ->
                let
                    x2 = Animation.animate time animation

                    updatedModel = {model |
                                        position = (x2, y),
                                        clock = time}
                in
                    if Animation.isDone time animation then
                        onAnimationComplete updatedModel

                    else
                        Effects.return updatedModel

            Just ("y", animation) ->
                let
                    y2 = Animation.animate time animation

                    updatedModel = {model |
                                        position = (x, y2),
                                        clock = time}
                in
                    if Animation.isDone time animation then
                        onAnimationComplete updatedModel

                    else
                        Effects.return updatedModel

            Just (_, _) ->
                Effects.return {model |
                                    clock = time}

            Nothing ->
                Effects.return {model |
                                    clock = time}

onAnimationComplete : Model -> Effects Model Effect
onAnimationComplete model =
    let
        (mbEntityEvent, updatedEventBuffer) =
            Deque.popFront model.eventBuffer
    in
        case mbEntityEvent of

            Just entityEvent ->
                let
                    entity = EntityEvent.entityEventEntity entityEvent

                    matrix = vertexMatrix entity.vertices

                    state = Deque.first updatedEventBuffer `Maybe.andThen` (
                                \entityEvent ->
                                    case entityEvent of

                                        EntityEvent.EntityMovedEv _ ->
                                            Maybe.Just Moving

                                        _ ->
                                            Maybe.Just Standing

                            ) |> Maybe.withDefault Standing
                in
                    queueEntityEvent {model |
                                        entity = entity,
                                        matrix = matrix,
                                        entityState = state,
                                        animation = Nothing,
                                        eventBuffer = updatedEventBuffer}

            Nothing ->
                Effects.return {model |
                                    entityState = Standing,
                                    animation = Nothing,
                                    eventBuffer = updatedEventBuffer}

popAndQueueEntityEvent : Model -> Effects Model Effect
popAndQueueEntityEvent model =
    let
        (_, poppedEventBuffer) = Deque.popFront model.eventBuffer
    in
        queueEntityEvent {model |
                            eventBuffer = poppedEventBuffer}

queueEntityEvent : Model -> Effects Model Effect
queueEntityEvent model =
    case model.animation of

        Just _ ->
            Effects.return model

        Nothing ->
            let
                cmd = Task.succeed model.entity
                        |> Task.perform NoOp ConsumeEntityEv
            in
                Effects.init model [PerformCmd cmd]

onEntitySpawnedEvent : EntitySpawnedEvent -> Model -> Effects Model Effect
onEntitySpawnedEvent entitySpawnedEvent model =
    let
        (_, poppedEventBuffer) = Deque.popFront model.eventBuffer

        matrix = vertexMatrix entitySpawnedEvent.entity.vertices

        position = entityPosition model.tmxMap matrix
    in
        Effects.return {model |
                            matrix = matrix,
                            position = position,
                            eventBuffer = poppedEventBuffer}

deltaPos : (Float, Float) -> (Float, Float) -> (Float, Float)
deltaPos (x1, y1) (x2, y2) =
    let
        deltaX = abs (x1 - x2)

        deltaY = abs (y1 - y2)
    in
        (deltaX, deltaY)

moveSpeed : String -> Float
moveSpeed entityType =
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
        (x1, y1) = model.position

        (x2, y2) = entityMovedEvent.entity.vertices
                        |> vertexMatrix
                        |> entityPosition model.tmxMap

        (deltaX, deltaY) = deltaPos model.position (x2, y2)

        speed = moveSpeed model.entity.entityType

        moveTime = 1000.0 - (1000.0 * speed)

        animation = if deltaX /= 0.0 then
                        let
                            animation =
                                Animation.animation model.clock
                                    |> Animation.from x1
                                    |> Animation.to x2
                                    |> Animation.duration moveTime
                                    |> Animation.ease Ease.linear
                        in
                            Maybe.Just ("x", animation)

                    else if deltaY /= 0.0 then
                        let
                            animation =
                                Animation.animation model.clock
                                    |> Animation.from y1
                                    |> Animation.to y2
                                    |> Animation.duration moveTime
                                    |> Animation.ease Ease.linear
                        in
                            Maybe.Just ("y", animation)

                    else
                        Maybe.Nothing
    in
        Effects.return {model |
            entity = entityMovedEvent.entity,
            entityState = Moving,
            animation = animation
        }

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

entityHeight : MinTmxMap -> Dict Int (List Int) -> Int
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

entityWidth : MinTmxMap -> Dict Int (List Int) -> Int
entityWidth tmxMap matrix =
    let
        colCount = entityColCount matrix
    in
        (colCount * tmxMap.tileWidth)

entityPosition : MinTmxMap -> Dict Int (List Int) -> (Float, Float)
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

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs OnAnimationFrame

-- View

entityAngle : String -> Float
entityAngle orientation =
    case orientation of

        "up" ->
            degrees 0

        "right" ->
            degrees 270

        "down" ->
            degrees 180

        "left" ->
            degrees 90

        _ ->
            degrees 0

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

        Dead ->
            Element.image 64 64
                "/static/assets/effects/blood/3-64x64.PNG"

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
        imageAngle = entityAngle model.entity.orientation
    in
        entityTypeImage model
            |> Collage.toForm
            |> Collage.rotate imageAngle

entityHealthBar : Model -> Collage.Form
entityHealthBar model =
    let
        health = model.entity.health
                    |> toFloat

        maxHealth = model.entity.maxHealth
                        |> toFloat

        healthPct = health / maxHealth

        width = entityWidth model.tmxMap model.matrix
                    |> toFloat

        offsetWidth = width - (width * 0.25)

        healthWidth = offsetWidth * healthPct

        healthColor =
            if healthPct < 0.25 then
                red
            else if healthPct < 0.5 then
                yellow
            else
                green
    in
        Collage.rect healthWidth 2.0
            |> Collage.filled healthColor

entityUuid : Model -> Collage.Form
entityUuid model =
    Element.show model.entity.uuid
        |> Collage.toForm

view : Model -> List Collage.Form
view model =
    let
        (x, y) = model.position

        backgroundForm = entityImage model

        -- healthBarForm = entityHealthBar model
        --
        -- entityForm = Collage.group [backgroundForm]

        -- uuid = entityUuid model
    in
        [backgroundForm]
            |> List.map (
                \form ->
                    Collage.move (x, y) form
            )
