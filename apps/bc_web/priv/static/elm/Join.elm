port module Join exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Json.Decode exposing (..)
import Message
import WebSocket

-- Model

type alias Model = {
	address : String
	playerId : Int
}

init : (Model, Cmd Msg)
init =
	(Model "" -1, Cmd.none)

type Msg =
	OnMessage String |
	Join String

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg {address, playerId} =
	case msg of

		Join handle ->

		OnMessage json ->
			case decodeString Message.message json of
				Ok Message ->
					-- TODO decompose join message
				Err Reason ->
					-- TODO show error message


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions {addres, _} =
	WebSocket.listen address OnMessage

-- Main

main : Program (Maybe Model)
main =
	App.programWithFlags {
		init = init,
		view = view,
		update = update,
		subscriptions = subscriptions
	}
