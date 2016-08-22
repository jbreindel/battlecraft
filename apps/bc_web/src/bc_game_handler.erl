
-module(bc_game_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opt) ->
	ViewFile = bc_web_files:view_file("game.html"),
	erlydtl:compile(ViewFile, game_view, 
					[{vars, [{title, "Game"}, 
							 {description, "Gameplay page"}]}]),
	{ok, Req, no_state}.

handle(Req, _State) ->
	{GameId, Req2} = cowboy_req:binding(game_id, Req),
	case game_view:render([{game_id, GameId}]) of
		{ok, View} ->
			{ok, cowboy_req:reply(200, [], View, Req), no_state};
		{error, ViewReason} ->
			{shutdown, cowboy_req:reply(500, [], <<ViewReason>>, Req), no_state}
	end.

terminate(_Reason, _Req, _State) ->
	ok.