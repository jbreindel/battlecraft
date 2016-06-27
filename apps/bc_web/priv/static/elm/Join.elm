port module Join exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import WebSocket

-- Model

type alias Model = {
	address : String
	playerId : Int
}

type Msg =
	OnMessage String

update : Msg -> Model -> (Model, Cmd Msg)
update msg {address, playerId} =
	case msg of
		OnMessage str ->
			(Model address )

subscriptions : Model -> Sub Msg
subscriptions {addres, _} =
	WebSocket.listen address OnMessage

main : Program (Maybe Model)
main =
	App.programWithFlags {
		init = init,
		view = view,
		update = update,
		subscriptions = subscriptions
	}
