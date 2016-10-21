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

start(_StartType, StartArgs) ->
	bc_model:init_model(),
	RoutesFile = bc_web_files:routes_file(),
	{ok, Routes} = file:consult(RoutesFile),
	Dispatch = cowboy_router:compile(Routes),
	{ok, Port} = application:get_env(http_port),
	cowboy:start_http(http, 100,
					  [{port, Port}],
					  [{env, [{dispatch, Dispatch}]}]),
    bc_web_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
