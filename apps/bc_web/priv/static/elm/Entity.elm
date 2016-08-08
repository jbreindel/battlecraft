module Entity exposing (Msg(..), Model)

import Element exposing (..)
import Collage exposing (..)
import Effects exposing (Effects)

-- Local imports

import EntityEvent exposing (Vertex, Entity, EntityEvent)

-- Actions

type Effect =
    Holder1 | Holder2

type Msg =
    EntityEv EntityEvent |
    NoOp

-- Model

type Orientation = Up | Right | Down | Left

type EntityState = Standing | Moving | Attacking

type alias Model = {
    entity : Entity,
    vertexMatrix : Dict Int List Int
    orientation : Orientation,
    entityState : EntityState
}

-- Init

init : Entity -> Effects Model Effect
init entity =
    Effects.return {
        entity = entity,
        orientation = Down,
        entityState = Standing
    }

-- View

entityRowCount : Dict -> Int
entityRowCount vertexMatrix =
    let
        rows = Dict.keys vertexMatrix
    in
        List.length rows

entityHeight : TmxMap -> Dict -> Int
entityHeight tmxMap vertexMatrix =
    let
        entityRows = entityRowCount vertexMatrix
    in
        entityRows * tmxMap.tileHeight

entityColCount : Dict -> Int
entityColCount vertexMatrix =
    let
        row = Dict.keys vertexMatrix
                |> List.take 1

        mbCols = Dict.get row vertexMatrix
    in
        case mbCols of
            Just cols ->
                List.length mbCols
            Nothing ->
                0

entityWidth : TmxMap -> Dict -> Int
entityWidth tmxMap vertexMatrix =
    let
        entityCol = entityColCount vertexMatrix
    in
        entityCol * tmxMap.tileWidth

-- entityPosition : TmxMap -> Dict -> (Int, Int)
-- entityPosition tmxMap vertexMatrix =
--     let
--         height = entityHeight tmxMap vertexMatrix
--
--         heightOffset = height / 2
--
--         width = entityWidth tmxMap vertexMatrix
--
--         widthOffset = entityWidth / 2
--     in
--

-- view : Model -> TmxMap -> Collage.Form
-- view model tmxMap =
--     let
--         entityType = model.entity.entityType
--
--         maxHealth = model.entity.maxHealth
--
--         healthPct = model.entity.health / maxHealth
--
--         entityWidth = entityWidth tmxMap.tileWidth model.vertexMatrix
--     in
--         case entityType of
--
--             "base" ->
