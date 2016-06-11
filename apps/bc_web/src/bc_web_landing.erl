
-module(bc_web_landing).
-behavior(cowboy_handler).

init(_Type, Req, _Opts) ->
	{ok, Req, no_state}.

handle(Req, State) ->
	{ok, {}, State}.

terminate(_Reason, Req, State) ->
	ok.