module Entity exposing (Msg(..), Model)

import EntityEvent exposing (Vertex, Entity, EntityEvent)

type Msg =
    EntityEv EntityEvent |
    NoOp

type alias Model = {
    entity : Entity
}
