port module Game exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (..)
import WebSocket

import Join

type GameState =
    Joining |
    Pending |
    Started

-- Action

type Msg =
    JoinMsg Join.Msg |
    StateChange GameState |
    OnWsMessage String

-- Model

type alias Model = {
    state : GameState
    joinModel : Join.Model
}

init : Model -> (Model, Cmd Msg)
init flags =
    (Model Joining (Join.init flags.address), Cmd.none)

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        JoinMsg sub ->
            let
                (joinModel, cmd) =
                    Join.update sub model.joinModel
            in
                ({model | joinModel = joinModel} , Cmd.map JoinMsg cmd)

        -- OnWsMessage str ->
            -- TODO decode message

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.address OnMessage

-- View

view : Model -> Html Msg
view model =
    -- let
    --     body =
    --         case model.state of
    --             Joining ->

    div [id "game"] []

-- Main

main : Program Model
main =
    App.programWithFlags {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }
