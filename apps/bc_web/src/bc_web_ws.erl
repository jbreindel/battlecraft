
-module(bc_web_ws).

-export([init/3, 
		 websocket_init/3, 
		 websocket_handle/3, 
		 websocket_info/3]).

-record(state, {game_id,
				player_id,
				player_serv}).

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

websocket_handle({text, Json} = Frame, Req, #state{game_id = GameId} = State) ->
	CommandMap = jsx:decode(Json, [return_maps]),
	case maps:get(<<"command_type">>, CommandMap, undefined) of
		<<"join_command">> ->
			Handle = maps:get(<<"handle">>, CommandMap),
			case join_game(GameId, Handle) of
				{ok, PlayerId, BcPlayerServ} ->
					ReplyMap = #{response_type => join_response,
								 player_id => PlayerId},
					ReplyJson = jsx:encode(ReplyMap),
					{reply, {text, ReplyJson}, Req, State#state{player_id = PlayerId}};
				{error, Reason} = Error ->
					ErrorMap = #{response_type => response_error,
								 error => Reason},
					ReplyJson = jsx:encode(ErrorMap),
					{reply, {text, ReplyJson, Req, State}}
			end;
		_ ->
			{reply, Frame, Req, State}
	end;
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info(Info, Req, State) when erlang:is_map(Info) ->
	{reply, {text, <<"Hello World">>}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

join_game(GameId, Handle) ->
	BcManagerServ = whereis(bc_manager_serv),
	case bc_manager_serv:get_game(GameId) of
		{ok, BcGameFsm} ->
			bc_game_fsm:player_join(BcGameFsm, self(), Handle);
		{error, Reason} = Error ->
			Error
	end.
