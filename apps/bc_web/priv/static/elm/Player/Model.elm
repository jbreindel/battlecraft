module Player.Model where

-- Model types

type alias Model = {
    id : Int,
    handle : String,
    isOut : Bool
}

initPlayer : Model
initPlayer =
    (Model -1 "" False)
