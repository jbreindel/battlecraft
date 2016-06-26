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

subscriptions : Model -> Sub Msg
subscriptions model =
	WebSocket.listen model.address NewMessage

main : Program(Maybe Model)
main =
	App.programWithFlags {
		init = init,
		view = view,
		update = update,
		subscriptions = subscriptions
	}
