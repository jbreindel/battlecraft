
-module(bc_web_games).

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
			{GameState, Req3} = cowboy_req:qs_val(<<"state">>, Req2, 1),
			case bc_game_model:get_games(GameState, 0, 25) of
				{ok, Games} ->
					case games_view:render([{games, Games}]) of
						{ok, View} ->
							{ok, cowboy_req:reply(200, [], View, Req3), no_state};
						{error, ViewReason} ->
							{shutdown, cowboy_req:reply(500, [], <<ViewReason>>, Req3), no_state}
					end;
				{error, GamesReason} ->
					{shutdown, cowboy_req:reply(500, [], <<GamesReason>>, Req3), no_state}
			end;
		{<<"POST">>, Req2} ->
			{ok, FormVals, Req3} = cowboy_req:body_qs(Req2),
			case proplists:get_value(<<"is_private">>, FormVals) of
				undefined ->
					%% TODO populate flashbag
					{shutdown, cowboy_req:reply(500, [], <<"Form invalid">>, Req3), no_state};
				IsPrivateStr ->
					BcManagerServ = whereis(bc_manager_serv),
					IsPrivate = case IsPrivateStr of <<"true">> -> true; <<"false">> -> false end,
					case bc_manager_serv:create_game(BcManagerServ, IsPrivate) of
						{ok, GameId, _} ->
							Location = string:concat("/game/", integer_to_list(GameId)),
							lager:info("Location: ~p", [Location]),
							Reply = cowboy_req:reply(303, [{<<"location">>, list_to_binary(Location)}], Req3),
							lager:info("Reply: ~p", [Reply]),
							{ok, Reply, no_state};
						{error, Reason} ->
							%% TODO populate flashbag
							{ok, cowboy_req:reply(303, [{<<"Location">>, <<"/games/">>}], Req3), no_state}
					end
			end
	end.

terminate(_Reason, _Req, _State) ->
	ok.