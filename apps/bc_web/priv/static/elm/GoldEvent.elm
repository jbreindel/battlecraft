
module GoldEvent exposing (GoldAccruedEvent,
                           goldAccruedEvent,
                           GoldSubtractedEvent,
                           goldSubtractedEvent,
                           GoldEvent,
                           goldEvent)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)

-- Model types

type alias GoldAccruedEvent = {
    eventType : String,
    gold : Int
}

goldAccruedEvent : Decoder GoldAccruedEvent
goldAccruedEvent =
    at ["gold_event"] <| succeed GoldAccruedEvent
        |: ("eventType" := string)
        |: ("gold" := int)

type alias GoldSubtractedEvent = {
    eventType : String,
    gold : Int
}

goldSubtractedEvent : Decoder GoldSubtractedEvent
goldSubtractedEvent =
    at ["gold_event"] <| succeed GoldSubtractedEvent
        |: ("eventType" := string)
        |: ("gold" := int)

-- Aggregate Types

type GoldEvent =
    GoldAccruedEv GoldAccruedEvent |
    GoldSubtractedEv GoldSubtractedEvent

goldEventInfo : String -> Decoder GoldEvent
goldEventInfo eventType =
    case eventType of

        "gold_accrued_event" ->
            object1 GoldAccruedEv goldAccruedEvent

        "gold_subtracted_event" ->
            object1 GoldSubtractedEv goldSubtractedEvent

        _ ->
            fail <| "Unable to parse eventType: " ++ eventType

goldEvent : Decoder GoldEvent
goldEvent =
    at ["gold_event", "event_type"] string `andThen` goldEventInfo
