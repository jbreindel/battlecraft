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
    StateChange GameState |
    JoinMsg Join.Msg |
    OnWsMessage String

-- Model

type alias Flags = {
    address : String
}

type alias Model = {
    state : GameState,
    address: String,
    joinModel : Join.Model
}

init : Flags -> (Model, Cmd Msg)
init flags =
    let
        (joinModel, joinCmd) = Join.init flags.address
    in
        (Model Joining flags.address joinModel, Cmd.none)

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        StateChange state ->
            ({model | state = state}, Cmd.none)

        JoinMsg sub ->
            let
                (updateJoinModel, updateJoinCmd) =
                    Join.update sub model.joinModel
            in
                ({model | joinModel = updateJoinModel}, Cmd.map JoinMsg updateJoinCmd)

        OnWsMessage str ->
            -- TODO decode message
            (model, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.address OnWsMessage

-- View

view : Model -> Html Msg
view model =
    let
        body =
            case model.state of

                Joining ->
                    App.map JoinMsg <| Join.view model.joinModel

                Pending ->
                    -- TODO create pending view
                    div [] []

                Started ->
                    -- TODO create map view
                    div [] []

    in
        div [id "game"] [
            body
        ]

-- Main

main : Program Flags
main =
    App.programWithFlags {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }
