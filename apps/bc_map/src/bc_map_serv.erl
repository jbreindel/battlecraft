
-module(bc_map_serv).
-behavior(gen_server).

%% api exports
-export([start_link/0, 
		 compute_path/2, 
		 are_neighbors/2]).

%% gen_server callbacks
-export([init/1, handle_call/3]).

-record(state, {map}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	gen_server:start_link({local, bc_map_serv}, bc_map_serv, [], []).

compute_path(Vertex1, Vertex2) ->
	gen_server:call(bc_map_serv, {path, Vertex1, Vertex2}).

are_neighbors(Vertex1, Vertex2) ->
	gen_server:call(bc_map_serv, {neighbors, Vertex1, Vertex2}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init(_Args) ->
	MapFileStr = filename:join([code:priv_dir(battlecraft), "map", "map.json"]),
	MapGraph = tmx:load_graph(MapFileStr),
	{ok, #state{map = MapGraph}}.

handle_call({path, Vertex1, Vertex2}, _From, State) ->
	#state{map = MapGraph} = State,
	try digraph:get_path(MapGraph, Vertex1, Vertex2) of
		false ->
			{reply, {error, "No such path"}, State};
		Verticies ->
			{reply, {ok, Verticies}}
	catch
		_ -> 
			{reply, {error, "Unable to compute path"}, State}
	end;

handle_call({neighbors, Vertex1, Vertex2}, _From, State) ->
	#state{map = MapGraph} = State,
	try map_utils:are_neighbors(MapGraph, Vertex1, Vertex2) of
		true ->
			{reply, true};
		false ->
			{reply, false}
	catch
		_ ->
			{reply, {error, "Unable to determine neighbors"}, State}
	end.
