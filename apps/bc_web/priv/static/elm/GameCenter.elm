
module GameCenter exposing (Effect(..),
                            Msg(..),
                            Model,
                            init)

import Effects exposing (Effects)

-- Local imports

import GameEvent exposing (GameEvent)

-- Actions

type Effect =
    UpdateGameState GameState

type Msg = 
    GameEv GameEvent

-- Model
    
type alias Model = {
    players : List Players,
    messages : List GameEvent
}

init : Effects Model Effect
init =
    Effects.return {
        players = [],
        messages = []
    }
