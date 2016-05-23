
-module(bc_manager_serv).
-behavior(gen_server).
-include("../include/bc_game_state.hrl").
-include("bc_game.hrl").

%% api functions
-export([start_link/1,
		 create_game/2,
		 get_game/2,
		 remove_game/2
		]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/3
		]).

%% state rec
-record(state, {
				game_sup,
				games
				}).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcGameSup) ->
	gen_server:start_link({local, bc_manager_serv}, ?MODULE, BcGameSup, []).

create_game(Name, Privacy) ->
	gen_server:call(Name, {create_game, Privacy}).

get_game(Name, GameId) ->
	gen_server:call(Name, {get_game, GameId}).

remove_game(Name, GameId) ->
	gen_server:cast(Name, {remove_game, GameId}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init(BcGameSup) ->
	{ok, #state{game_sup = BcGameSup, games = dict:new()}}.
	
handle_call({create_game, Privacy}, _From, 
		S = #state{game_sup = BcGameSup, games = GameDict}) ->
	case new_game(Privacy) of
		{ok, GameId} ->
			{ok, BcGameFsm} = supervisor:start_child(BcGameSup, #{
			   id => GameId,
			   start => {bc_game_fsm, start_link, [BcGameSup]},
			   modules => [bc_game_fsm]
			}),
			{reply, {ok, GameId, BcGameFsm}, S#state{games = dict:store(GameId, BcGameFsm, GameDict)}};
		{error, Reason} ->
			{reply, {error, Reason}, S}
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

%%====================================================================
%% Internal functions
%%====================================================================

new_game(Privacy) ->
	Now = now(),
	GameId = bc_model:gen_id(game),
	Game = #game{id = GameId,
				 state = ?CREATED,
				 winner_id = 0,
				 is_private = Privacy,
				 created = Now,
				 modified = Now},
	case mnesia:sync_transaction(fun() -> mnesia:write(Game) end) of
		{atomic, Result} ->
			{ok, GameId};
		{aborted, Reason} ->
			{error, Reason}
	end.

