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

entityRowCount : Dict Int (List Int) -> Int
entityRowCount matrix =
    let
        rows = Dict.keys matrix
    in
        List.length rows

entityHeight : TmxMap -> Dict Int (List Int) -> Int
entityHeight tmxMap matrix =
    let
        entityRows = entityRowCount matrix
    in
        entityRows * tmxMap.tileHeight

entityColCount : Dict Int (List Int) -> Int
entityColCount matrix =
    let
        row = Dict.keys matrix
                |> List.head
                |> Maybe.withDefault -1

        cols = Dict.get row matrix
                |> Maybe.withDefault []
    in
        List.length cols

entityWidth : TmxMap -> Dict Int (List Int) -> Int
entityWidth tmxMap matrix =
    let
        colCount = entityColCount matrix
    in
        colCount * tmxMap.tileWidth

entityPosition : TmxMap -> Dict Int (List Int) -> (Float, Float)
entityPosition tmxMap matrix =
    let
        minRow = Dict.keys matrix
                    |> List.minimum
                    |> Maybe.withDefault -1

        minCol = Dict.get minRow matrix
                    |> Maybe.withDefault []
                    |> List.minimum
                    |> Maybe.withDefault -1

        height = entityHeight tmxMap matrix
                    |> toFloat

        heightOffset = height / 2

        width = entityWidth tmxMap matrix
                    |> toFloat

        widthOffset = width / 2

        tileHeight = tmxMap.tileHeight
                        |> toFloat

        y = ((toFloat minRow) * tileHeight) / 2

        offsetY = y + heightOffset

        tileWidth = tmxMap.tileWidth
                        |> toFloat

        x = ((toFloat minCol) * tileWidth) / 2

        offsetX = x + widthOffset
    in
        (offsetX, offsetY)

-- View

{--

view : Model -> TmxMap -> Collage.Form
view model tmxMap =
    let
        entityType = model.entity.entityType

        maxHealth = model.entity.maxHealth

        healthPct = model.entity.health / maxHealth

        position = entityPosition tmxMap model.matrix
    in
       case entityType of

           "base" ->

--}
