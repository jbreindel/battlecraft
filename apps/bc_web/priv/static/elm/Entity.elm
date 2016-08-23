
module Entity exposing (Effect(..),
                        Msg(..),
                        Model,
                        init,
                        update,
                        view)

import Dict exposing (..)
import Color exposing (green, orange, red)
import Element exposing (..)
import Collage exposing (..)
import Effects exposing (Effects)

-- Local imports

import TmxMap exposing (TmxMap, tmxMapHeight, tmxMapWidth)
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
    tmxMap : TmxMap,
    matrix : Dict Int (List Int),
    position : (Float, Float),
    orientation : Orientation,
    entityState : EntityState
}

init : TmxMap -> Entity -> Effects Model Effect
init tmxMap entity =
        Effects.return {
            entity = entity,
            tmxMap = tmxMap,
            matrix = Dict.empty,
            position = (0.0, 0.0),
            orientation = Down,
            entityState = Standing
        }

-- Update

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

                matrix = vertexMatrix vertices

                position = entityPosition model.tmxMap matrix
            in
                Effects.return {model |
                                    matrix = matrix,
                                    position = position}

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

entityBackgroundImage : Model -> Collage.Form
entityBackgroundImage model =
    let
        image = case model.entity.entityType of

                    "base" ->
                        Element.empty

                    _ ->
                        Element.empty
    in
        image |> Collage.toForm

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
        Collage.rect offsetWidth 10.0
            |> filled green

view : Model -> Collage.Form
view model =
    let
        (x, y) = model.position

        backgroundForm = entityBackgroundImage model

        healthBarForm = entityHealthBar model

        entityForm = Collage.group [backgroundForm, healthBarForm]
    in
       Collage.move (x, y) entityForm
