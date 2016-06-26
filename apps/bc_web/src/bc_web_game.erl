
-module(bc_web_game).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opt) ->
	ViewFile = bc_web_files:view_file("game.html"),
	erlydtl:compile(ViewFile, games_view, 
					[{vars, [{title, "Game"}, 
							 {description, "Gameplay page"}]}]),
	{ok, Req, no_state}.

handle(Req, _State) ->
	case games_view:render([]) of
		{ok, View} ->
			{ok, cowboy_req:reply(200, [], View, Req), no_state};
		{error, ViewReason} ->
			{shutdown, cowboy_req:reply(500, [], <<ViewReason>>, Req), no_state}
	end.

terminate(_Reason, _Req, _State) ->
	ok.