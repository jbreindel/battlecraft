%%%-------------------------------------------------------------------
%% @doc bc_web public API
%% @end
%%%-------------------------------------------------------------------

-module(bc_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	RoutesFile = filename:join([code:priv_dir(bc_web), "bc_web.routes"]),
	{ok, Routes} = file:consult(RoutesFile),
	Dispatch = cowboy_router:compile(Routes),
	cowboy:start_http(http, 100,
					  [{port, 8080}],
					  [{env, [{dispatch, Dispatch}]}]),
    bc_web_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
