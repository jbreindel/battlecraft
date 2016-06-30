module Join exposing (..)

-- Model

type alias Model = {
    address : String,
    handle : String,
    playerId : Int
}

init : Model -> (Model, Cmd Msg)
init savedModel =
    (Model savedModel.address "" -1, Cmd.none)

-- Update

type Msg =
    JoinGame |
    UpdateHandle String |
    OnMessage String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        UpdateHandle handle ->
            ({model | handle = handle}, Cmd.none)

        JoinGame ->
            (model, WebSocket.send model.address model.handle)

        OnMessage json ->
            (model, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.address OnMessage

-- View

view : Model -> Html Msg
view model =
    div []
        [
            input [placeholder "Game Handle", onInput UpdateHandle] [],
            button [onClick JoinGame] [text "Join"]
