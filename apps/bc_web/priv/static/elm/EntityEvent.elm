
module EntityEvent exposing (EntityEvent(..),
                             Vertex,
                             Entity,
                             EntitySpawnedEvent,
                             EntityDamagedEvent,
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

type alias EntityDamagedEvent = {
    eventType : String,
    entity : Entity
}

entityDamagedEvent : Decoder EntityDamagedEvent
entityDamagedEvent =
    at ["entity_event"] <| succeed EntityDamagedEvent
        |: ("event_type" := string)
        |: ("entity" := entity)

-- Aggregate Types

type EntityEvent =
    EntitySpawnedEv EntitySpawnedEvent |
    EntityDamagedEv EntityDamagedEvent

entityEventInfo : String -> Decoder EntityEvent
entityEventInfo eventType =
    case eventType of

        "entity_spawned" ->
            object1 EntitySpawnedEv entitySpawnedEvent

        "entity_damaged" ->
            object1 EntityDamagedEv entityDamagedEvent

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

        EntityDamagedEv entityDamagedEvent ->
            entityDamagedEvent.entity
