
-module(bc_web_games).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opt) ->
	ViewFile = bc_web_files:view_file("games.html"),
	erlydtl:compile(ViewFile, games_view, 
					[{vars, [{title, "Games"}, 
							 {description, "List of games to join"}]}]),
	{ok, Req, no_state}.

handle(Req, _State) ->
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

terminate(_Reason, _Req, _State) ->
	ok.