
-module(bc_model).

%% API exports
-export([init/0, migrate/0, rebuild/0]).

-define(APPNAME, battlecraft).
-define(MODELS, [list_to_atom(M) || M <- ["src/bc_player.erl", 
										  "src/bc_game.erl"]]).

%%====================================================================
%% API functions
%%====================================================================

init() ->
	init_db(),
	ok.

migrate() ->
    mnesia:stop(),
    init().

rebuild() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    init().

%%====================================================================
%% Internal functions
%%====================================================================

model_list() ->
	

init_db() ->
  init_db([node()]). % only for local node? what about nodes()++[node()]?
init_db(Nodes) ->
  mnesia:create_schema(Nodes),
  mnesia:change_table_copy_type(schema, node(), disc_copies), % only for local node?
  mnesia:start(),
  ModelList = [list_to_atom (M) || M <- boss_files:model_list(?APPNAME)],
  ExistingTables = mnesia:system_info(tables),
  Tables = (ModelList ++ ['_ids_']) -- ExistingTables,
  create_model_tables(Nodes, Tables).

table_copy_type([]) ->
	ok;
table_copy_type([Node | Nodes]) ->
  mnesia:change_table_copy_type(schema, Node, disc_copies),
  table_copy_type(Nodes).

% create all the tables
create_model_tables(_, []) -> ok;
create_model_tables(Nodes, [Model | Models]) ->
  [create_model_table(Nodes, Model)] ++
   create_model_tables(Nodes, Models).

% specific tables (not generated from model)
create_model_table (Nodes, '_ids_') ->
  create_table (Nodes, '_ids_', [type, id]);

% tables generated from model
create_model_table(Nodes, Model) ->
  Record = boss_record_lib:dummy_record(Model),
  { Model, create_table(Nodes, Model, Record:attribute_names()) }.

% single table creator wrapper
create_table(Nodes, Table, Attribs) ->
  mnesia:create_table(Table,
    [{ disc_copies, Nodes },
      { attributes,  Attribs }]).
