
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
        Debug.log "entityRowCount: " (List.length rows)

entityHeight : TmxMap -> Dict Int (List Int) -> Int
entityHeight tmxMap matrix =
    let
        entityRows = entityRowCount matrix
    in
        Debug.log "entityHeight: " (entityRows * tmxMap.tileHeight)

entityColCount : Dict Int (List Int) -> Int
entityColCount matrix =
    let
        row = Dict.keys matrix
                |> List.head
                |> Maybe.withDefault -1

        cols = Dict.get row matrix
                |> Maybe.withDefault []
    in
        Debug.log "entityColCount: " (List.length cols)

entityWidth : TmxMap -> Dict Int (List Int) -> Int
entityWidth tmxMap matrix =
    let
        colCount = entityColCount matrix
    in
        Debug.log "entityWidth: " (colCount * tmxMap.tileWidth)

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

        mapHeight = tmxMapHeight tmxMap
                        |> toFloat

        tileHeight = tmxMap.tileHeight
                        |> toFloat

        y = (mapHeight - ((toFloat minRow) * tileHeight)) / 2

        offsetY = y - heightOffset

        mapWidth = tmxMapWidth tmxMap
                        |> toFloat

        tileWidth = tmxMap.tileWidth
                        |> toFloat

        x = (mapWidth - ((toFloat minCol) * tileWidth)) / 2

        offsetX = x - widthOffset
    in
        Debug.log "position: " (offsetX, offsetY)

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

entityHealthBar : Model -> TmxMap -> Collage.Form
entityHealthBar model tmxMap =
    let
        width = entityWidth tmxMap model.matrix
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

view : Model -> TmxMap -> Collage.Form
view model tmxMap =
    let
        backgroundForm = entityBackgroundImage model

        healthBarForm = entityHealthBar model tmxMap

        entityForm = Collage.group [backgroundForm, healthBarForm]

        (x, y) = entityPosition tmxMap model.matrix
    in
       Collage.move (x, y) entityForm
