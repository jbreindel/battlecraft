
-module(bc_game_ws).

-export([init/3, 
		 websocket_init/3, 
		 websocket_handle/3, 
		 websocket_info/3,
		 websocket_terminate/3]).

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
	lager:info("Websocket message received: ~p~n", [CommandMap]),
	case maps:get(<<"command_type">>, CommandMap, undefined) of
		<<"join_command">> ->
			Handle = maps:get(<<"handle">>, CommandMap),
			case join_game(GameId, Handle) of
				{ok, PlayerId, Team, BcPlayerServ} ->
					ReplyMap = #{type => command_response,
								 command_response => #{response_type => join_response,
													   player_id => PlayerId,
													   team => Team}},
					ReplyJson = jsx:encode(ReplyMap),
					{reply, {text, ReplyJson}, Req, State#state{player_id = PlayerId}};
				{error, Reason} = Error ->
					io:format("Error occured while joining: ~p~n", [Error]),
					ErrorMap = #{type => command_response,
								 command_response => #{response_type => join_error, 
													   error => Reason}},
					ReplyJson = jsx:encode(ErrorMap),
					{reply, {text, ReplyJson}, Req, State}
			end;
		undefined ->
			{reply, Frame, Req, State}
	end;
websocket_handle(Frame, Req, State) ->
	lager:info("Websocket message receive: ~p", [Frame]),
	{ok, Req, State}.

websocket_info(WsMessage, Req, State) when erlang:is_map(WsMessage) ->
	lager:info("Serializing ws message: ~p", [WsMessage]),
	Json = jsx:encode(WsMessage),
	{reply, {text, Json}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
	lager:info("Websocket terminating: ~p", [Reason]),
    ok.

join_game(GameId, Handle) ->
	BcManagerServ = whereis(bc_manager_serv),
	case bc_manager_serv:get_game(BcManagerServ, GameId) of
		{ok, BcGameFsm} ->
			bc_game_fsm:player_join(BcGameFsm, self(), Handle);
		{error, Reason} = Error ->
			Error
	end.