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

-- view : Map.Model Model -> Collage.Form
-- view model TmxMap =
--     let
--         entityType = model.entity.entityType
--
--         maxHealth = model.entity.maxHealth
--
--         healthPct = model.entity.health / maxHealth
--
--         vertices = model.entity.vertices
--     in
--         case entityType of
--
--             "base" ->
