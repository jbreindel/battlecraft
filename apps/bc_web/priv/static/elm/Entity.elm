module Entity exposing (Effect(..), Msg(..), Model, init)

import Dict exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Effects exposing (Effects)

-- Local imports

import TmxMap exposing (TmxMap)
import EntityEvent exposing (Vertex, Entity, EntityEvent)

-- Actions

type Effect =
    Holder1 | Holder2

type Msg =
    EntityEv EntityEvent |
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
    matrix : Dict Int (List Int),
    position : (Float, Float),
    orientation : Orientation,
    entityState : EntityState
}

init : TmxMap -> Entity -> Effects Model Effect
init tmxMap entity =
        Effects.return {
            entity = entity,
            matrix = Dict.empty,
            position = (0.0, 0.0),
            orientation = Down,
            entityState = Standing
        }

-- Update

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

update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of

        EntityEv entityEvent ->
            onEntityEvent entityEvent model

        NoOp ->
            Effects.return model

onEntityEvent : EntityEvent -> Model -> Effects Model Effect
onEntityEvent entityEvent model =
    case entityEvent of

        EntityEvent.EntitySpawnedEv entitySpawnedEvent ->
            let
                vertices = entitySpawnedEvent.entity.vertices
            in
                Effects.return {model |
                                    matrix = vertexMatrix vertices}

        EntityEvent.EntityDamagedEv entityDamagedEvent ->
            Effects.return {model |
                                entity = entityDamagedEvent.entity}

{--

entityRowCount : Dict Int List Int -> Int
entityRowCount vertexMatrix =
    let
        rows = Dict.keys vertexMatrix
    in
        List.length rows

entityHeight : TmxMap -> Dict Int List Int -> Int
entityHeight tmxMap vertexMatrix =
    let
        entityRows = entityRowCount vertexMatrix
    in
        entityRows * tmxMap.tileHeight

entityColCount : Dict Int List Int -> Int
entityColCount vertexMatrix =
    let
        row = Dict.keys vertexMatrix
                |> List.take 1
    in
        Dict.get row vertexMatrix
            |> Maybe.withDefault 0

entityWidth : TmxMap -> Dict Int List Int -> Int
entityWidth tmxMap vertexMatrix =
    let
        entityCol = entityColCount vertexMatrix
    in
        entityCol * tmxMap.tileWidth

entityPosition : TmxMap -> Dict Int List Int -> (Float, Float)
entityPosition tmxMap vertexMatrix =
    let
        minRow = Dict.keys vertexMatrix
                    |> List.minimum
                    |> Maybe.withDefault -1

        minCol = Dict.get minRow vertexMatrix
                    |> Maybe.withDefault [-1]
                    |> List.minimum

        height = entityHeight tmxMap vertexMatrix

        heightOffset = height / 2

        width = entityWidth tmxMap vertexMatrix

        widthOffset = entityWidth / 2

        y = ((minRow * tmxMap.tileHeight) / 2)

        offsetY = y + heightOffset

        x = ((minCol * tmxMap.tileWidth) / 2)

        offsetX = x + widthOffset
    in
        (toFloat offsetX, toFloat offsetY)

--}

-- View

{--

view : Model -> TmxMap -> Collage.Form
view model tmxMap =
    let
        entityType = model.entity.entityType

        maxHealth = model.entity.maxHealth

        healthPct = model.entity.health / maxHealth

        entityWidth = entityWidth tmxMap.tileWidth model.vertexMatrix
    in
       case entityType of

           "base" ->

--}
