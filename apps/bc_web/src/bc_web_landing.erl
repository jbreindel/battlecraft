
-module(bc_web_landing).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
	ViewFile = bc_web_files:view_file("landing.html"),
	erlydtl:compile(ViewFile, landing_view, 
					[{vars, [{title, "Battlecraft Online"}, 
							 {description, "An online real time strategy simulation game."}]}]),
	{ok, Req, no_state}.

handle(Req, State) ->
	case landing_view:render([]) of
		{ok, View} ->
		    Req2 = cowboy_req:reply(200, [
    			{<<"content-type">>, <<"text/html">>}
			], View, Req),
			{ok, Req2, State};
		{error, Reason} ->
			Req2 = cowboy_req:reply(500, [
    			{<<"content-type">>, <<"text/plain">>}
			], <<Reason>>, Req),
			{shutdown, Req2, no_state}
	end.

terminate(_Reason, Req, State) ->
	ok.