
-module(bc_map_serv).
-behavior(gen_server).

%% api exports
-export([start_link/0, 
		 compute_path/2, 
		 are_neighbors/2]).

%% gen_server callbacks
-export([init/1, handle_call/3]).

%% vertex inside the graph
-type vertex() :: #{row => integer(),
					col => integer()}.

%% internal state
-record(state, {map}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	gen_server:start_link(?MODULE, bc_map_serv, [], []).

compute_path(BcMapServ, Vertex1, Vertex2) ->
	gen_server:call(BcMapServ, {path, Vertex1, Vertex2}).

are_neighbors(BcMapServ, Vertex1, Vertex2) ->
	gen_server:call(BcMapServ, {neighbors, Vertex1, Vertex2}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init() ->
	MapFileStr = filename:join([code:priv_dir(battlecraft), "map", "map.json"]),
	{ok, Json} = file:read_file(MapFileStr),
	TmxJsonMap = jsx:decode(Json, [return_maps]),
	MapGraph = bc_tmx:load_graph(TmxJsonMap),
	{ok, #state{map = MapGraph}}.

handle_call({path, Vertex1, Vertex2}, _From, #state{map = MapGraph} = State) ->
	case digraph:get_path(MapGraph, Vertex1, Vertex2) of
		Verticies when erlang:is_list(Verticies) ->
			{reply, {ok, Verticies}, State};
		false ->
			{reply, {error, "No such path"}, State}
	end;

handle_call({neighbors, Vertex1, Vertex2}, _From, #state{map = MapGraph} = State) ->
	case bc_map_utils:are_neighbors(MapGraph, Vertex1, Vertex2) of
		true ->
			{reply, true};
		false ->
			{reply, false}
	end.
