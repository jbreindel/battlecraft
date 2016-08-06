module Entity exposing (Msg(..), Model)

import Element exposing (..)
import Collage exposing (..)
import Effects exposing (Effects)

-- Local imports

import EntityEvent exposing (Vertex, Entity, EntityEvent)

-- Actions

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

-- init : Entity -> Model
-- init entity =
--     Effects.return {
--         entity = entity,
--         orientation = Down,
--         entityState = Standing
--     }
