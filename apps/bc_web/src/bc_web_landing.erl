
-module(bc_web_landing).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
	{ok, Req, no_state}.

handle(Req, State) ->
	{ok, {}, State}.

terminate(_Reason, Req, State) ->
	ok.