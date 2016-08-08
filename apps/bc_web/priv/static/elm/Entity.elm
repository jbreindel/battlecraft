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
    vertexMatrix : Dict Int List Int,
    position : (Float, Float),
    orientation : Orientation,
    entityState : EntityState
}

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
    in
        Dict.get row vertexMatrix
            |> Maybe.withDefault 0

entityWidth : TmxMap -> Dict -> Int
entityWidth tmxMap vertexMatrix =
    let
        entityCol = entityColCount vertexMatrix
    in
        entityCol * tmxMap.tileWidth

entityPosition : TmxMap -> Dict -> (Float, Float)
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
