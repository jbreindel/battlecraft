module GameErrorEvent.Model where

type alias Model = {
    eventType : String,
    reason : String
}

initGameErrorEvent : Model
initGameErrorEvent =
    (Model "" "")
