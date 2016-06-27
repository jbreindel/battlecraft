
-module(bc_web_ws).

-export([init/3]).

-record(state, {game_id, 
				input_serv}).

init(_Type, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
	{GameId, Req2} = cowboy_req:binding(game_id, Req),
	case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req2) of
		{ok, undefined, Req3} ->
			{ok, Req2, #state{game_id = GameId}};
		{ok, Subprotocols, Req3} ->
			case lists:keymember(<<"game">>, 1, Subprotocols) of
				true ->
					Req4 = cowboy_req:set_resp_header(
							 <<"sec-websocket-protocol">>, <<"game">>, Req3),
					{ok, Req4, #state{game_id = GameId}};
				false ->
					{shutdown, Req3}
			end
	end.

websocket_handle({text, Json} = Frame, Req, State) ->
	{reply, Frame, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info(Info, Req, State) when erlang:is_map(Info) ->
	{reply, {text, <<"Hello World">>}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.