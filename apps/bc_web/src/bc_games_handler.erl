
-module(bc_games_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opt) ->
	ViewFile = bc_web_files:view_file("games.html"),
	erlydtl:compile(ViewFile, games_view, 
					[{vars, [{title, "Games"}, 
							 {description, "List of games to join"}]}]),
	{ok, Req, no_state}.

handle(Req, _State) ->
	case cowboy_req:method(Req) of
		{<<"GET">>, Req2} ->
			handle_get(Req2);
		{<<"POST">>, Req2} ->
			handle_post(Req2)
	end.

terminate(_Reason, _Req, _State) ->
	ok.

handle_get(Req) ->
	{GameState, Req2} = cowboy_req:qs_val(<<"state">>, Req, 1),
	case bc_game_model:get_games(GameState, 0, 25) of
		{ok, Games} ->
			case games_view:render([{games, Games}]) of
				{ok, View} ->
					{ok, cowboy_req:reply(200, [], View, Req2), no_state};
				{error, ViewReason} ->
					{shutdown, cowboy_req:reply(500, [], <<ViewReason>>, Req2), no_state}
			end;
		{error, GamesReason} ->
			{shutdown, cowboy_req:reply(500, [], <<GamesReason>>, Req2), no_state}
	end.

handle_post(Req) ->
	{ok, FormVals, Req2} = cowboy_req:body_qs(Req),
	case proplists:get_value(<<"is_private">>, FormVals) of
		undefined ->
			%% TODO populate flashbag
			{shutdown, cowboy_req:reply(500, [], <<"Form invalid">>, Req2), no_state};
		IsPrivateStr ->
			case proplists:get_value(<<"max_players">>, FormVals) of
				undefined ->
					%% TODO populate flashbag
					{shutdown, cowboy_req:reply(500, [], <<"Form invalid">>, Req2), no_state};
				MaxPlayersStr ->
					IsPrivate = case IsPrivateStr of <<"true">> -> true; <<"false">> -> false end,
					MaxPlayers = case MaxPlayersStr of <<"2">> -> 2; <<"4">> -> 4 end,
					create_game(IsPrivate, MaxPlayers, Req2)
			end
	end.

create_game(IsPrivate, MaxPlayers, Req) ->
	BcManagerServ = whereis(bc_manager_serv),
	case bc_manager_serv:create_game(BcManagerServ, IsPrivate, MaxPlayers) of
		{ok, GameId, _} ->
			Location = string:concat("/game/", integer_to_list(GameId)),
			Reply = cowboy_req:reply(303, [{<<"location">>, list_to_binary(Location)}], Req),
			{ok, Reply, no_state};
		{error, Reason} ->
			%% TODO populate flashbag
			{ok, cowboy_req:reply(303, [{<<"Location">>, <<"/games/">>}], Req), no_state}
	end.