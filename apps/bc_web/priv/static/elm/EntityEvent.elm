
module EntityEvent exposing (EntityEvent(..),
                             Vertex,
                             Entity,
                             EntitySpawnedEvent,
                             EntityMovedEvent,
                             EntityDamagedEvent,
                             EntityAttackingEvent,
                             EntityDiedEvent,
                             entityEvent,
                             entityEventEntity)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)

-- Model types

type alias Vertex = {
    row : Int,
    col : Int
}

vertex : Decoder Vertex
vertex =
    succeed Vertex
        |: ("row" := int)
        |: ("col" := int)

type alias Entity = {
    uuid : String,
    entityType : String,
    health : Int,
    maxHealth : Int,
    playerId : Int,
    playerNum : Int,
    team : Int,
    orientation : String,
    vertices : List Vertex
}

entity : Decoder Entity
entity =
    succeed Entity
        |: ("uuid_str" := string)
        |: ("entity_type" := string)
        |: ("health" := int)
        |: ("max_health" := int)
        |: ("player_id" := int)
        |: ("player_num" := int)
        |: ("team" := int)
        |: ("orientation" := string)
        |: ("vertices" := list vertex)

-- Event Types

type alias EntitySpawnedEvent = {
    eventType : String,
    entity : Entity
}

entitySpawnedEvent : Decoder EntitySpawnedEvent
entitySpawnedEvent =
    at ["entity_event"] <| succeed EntitySpawnedEvent
        |: ("event_type" := string)
        |: ("entity" := entity)

type alias EntityMovedEvent = {
    eventType : String,
    entity : Entity
}

entityMovedEvent : Decoder EntityMovedEvent
entityMovedEvent =
    at ["entity_event"] <| succeed EntityMovedEvent
        |: ("event_type" := string)
        |: ("entity" := entity)

type alias EntityDamagedEvent = {
    eventType : String,
    entity : Entity
}

entityDamagedEvent : Decoder EntityDamagedEvent
entityDamagedEvent =
    at ["entity_event"] <| succeed EntityDamagedEvent
        |: ("event_type" := string)
        |: ("entity" := entity)

type alias EntityAttackingEvent = {
    eventType : String,
    entity : Entity
}

entityAttackingEvent : Decoder EntityAttackingEvent
entityAttackingEvent =
    at ["entity_event"] <| succeed EntityAttackingEvent
        |: ("event_type" := string)
        |: ("entity" := entity)

type alias EntityDiedEvent = {
    eventType : String,
    entity : Entity
}

entityDiedEvent : Decoder EntityDiedEvent
entityDiedEvent =
    at ["entity_event"] <| succeed EntityDiedEvent
        |: ("event_type" := string)
        |: ("entity" := entity)

-- Aggregate Types

type EntityEvent =
    EntitySpawnedEv EntitySpawnedEvent |
    EntityMovedEv EntityMovedEvent |
    EntityDamagedEv EntityDamagedEvent |
    EntityAttackingEv EntityAttackingEvent |
    EntityDiedEv EntityDiedEvent

entityEventInfo : String -> Decoder EntityEvent
entityEventInfo eventType =
    case eventType of

        "entity_spawned" ->
            object1 EntitySpawnedEv entitySpawnedEvent

        "entity_moved" ->
            object1 EntityMovedEv entityMovedEvent

        "entity_damaged" ->
            object1 EntityDamagedEv entityDamagedEvent

        "entity_attacking" ->
            object1 EntityAttackingEv entityAttackingEvent

        "entity_died" ->
            object1 EntityDiedEv entityDiedEvent

        _ ->
            fail <| "Unable to parse eventType: " ++ eventType

entityEvent : Decoder EntityEvent
entityEvent =
    at ["entity_event", "event_type"] string `andThen` entityEventInfo

-- Helper Funs

entityEventEntity : EntityEvent -> Entity
entityEventEntity entityEvent =
    case entityEvent of

        EntitySpawnedEv entitySpawnedEvent ->
            entitySpawnedEvent.entity

        EntityMovedEv entityMovedEvent ->
            entityMovedEvent.entity

        EntityDamagedEv entityDamagedEvent ->
            entityDamagedEvent.entity

        EntityAttackingEv entityAttackingEvent ->
            entityAttackingEvent.entity

        EntityDiedEv entityDiedEvent ->
            entityDiedEvent.entity
