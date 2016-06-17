
-module(bc_manager_serv).
-behavior(gen_server).
-include("../include/bc_game_state.hrl").

%% api functions
-export([start_link/0,
		 create_game/2,
		 get_game/2,
		 remove_game/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/3]).

%% state rec
-record(state, {manager_sup,
				games}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	gen_server:start_link({local, bc_manager_serv}, ?MODULE, [], []).

create_game(Name, Privacy) ->
	gen_server:call(Name, {create_game, Privacy}).

get_game(Name, GameId) ->
	gen_server:call(Name, {get_game, GameId}).

remove_game(Name, GameId) ->
	gen_server:cast(Name, {remove_game, GameId}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init([]) ->
	BcManagerSup = whereis(bc_manager_sup),
	{ok, #state{manager_sup = BcManagerSup, games = dict:new()}}.
	
handle_call({create_game, Privacy}, _From, 
	#state{manager_sup = BcManagerSup, games = GameDict} = State) ->
	case bc_game_model:save(Privacy, ?PENDING) of
		{ok, GameId} ->
			{ok, BcGameSup} = supervisor:start_child(BcManagerSup, #{
				 id => GameId,
				 start => {bc_game_sup, start_link, []},
				 type => supervisor,
				 modules => [bc_game_sup]
			}),
			{ok, BcInputSup} = supervisor:start_child(BcGameSup, #{
				id => bc_input_sup,
				start => {bc_input_sup, start_link, []},
				modules => [bc_input_sup]
			}),
			{ok, GameEventPid} = supervisor:start_child(BcGameSup, #{
				id => bc_game_event,
				start => {gen_event, start_link, []},
				modules => [gen_event]
			}),
			{ok, BcGameFsm} = supervisor:start_child(BcGameSup, #{
			   id => bc_game_fsm,
			   start => {bc_game_fsm, start_link, [GameId, GameEventPid, BcInputSup]},
			   modules => [bc_game_fsm]
			}),
 			{reply, {ok, GameId, BcGameFsm}, 
			 State#state{games = dict:store(GameId, BcGameFsm, GameDict)}};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;

handle_call({get_game, GameId}, _From, State) ->
	GameDict = State#state.games,
	case dict:find(GameId, GameDict) of
		{ok, BcGameFsm} ->
			{reply, {ok, BcGameFsm}, State};
		error ->
			{reply, {error, not_found}, State}
	end.

handle_cast({remove_game, GameId}, _From, State) ->
	GameDict = State#state.games,
	{noreply, State#state{games = dict:erase(GameId, GameDict)}}.

