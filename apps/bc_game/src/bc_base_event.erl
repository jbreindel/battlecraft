
-module(bc_base_event).
-behavior(gen_event).

-export([init/1, 
		 handle_event/2,
		 handle_call/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

%% internal state
-record(state, {base_uuid, 
				game, 
				player}).

%%====================================================================
%% Public functions
%%====================================================================

-spec init(InitArgs) -> Result when
	InitArgs :: Args | {Args, Term :: term()},
	Args :: term(),
	Result :: {ok, State}
			| {ok, State, hibernate}
			| {error, Reason :: term()},
	State :: term().
init([BaseUuid, BcGame, BcPlayer]) ->
	{ok, #state{base_uuid = BaseUuid, 
				game = BcGame,
				player = BcPlayer}}.

-spec handle_event(Event :: term(), State :: term()) -> Result when
	Result :: {ok, NewState}
			| {ok, NewState, hibernate}
			| {swap_handlers, Args1, NewState, Handler2, Args2}
			| remove_handler,
	NewState :: term(), Args1 :: term(), Args2 :: term(),
	Handler2 :: Module2 | {Module2, Id :: term()},
	Module2 :: atom().
handle_event({entity_died, BcEntity}, #state{base_uuid = BaseUuid, 
											  game = BcGame, 
											  player = BcPlayer} = State) ->
	case BaseUuid == bc_entity:uuid(BcEntity) of
		true ->
			GameFsmPid = bc_game:fsm(BcGame),
			PlayerId = bc_player:id(BcPlayer),
			bc_game_fsm:player_out(GameFsmPid, PlayerId),
			remove_handler;
		false ->
			{ok, State}
	end;
handle_event(_, State) ->
	{ok, State}.

-spec handle_call(Request :: term(), State :: term()) -> Result when
	Result :: {ok, Reply, NewState}
			| {ok, Reply, NewState, hibernate}
			| {swap_handler, Reply, Args1, NewState, Handler2, Args2}
			| {remove_handler, Reply},
	Reply :: term(),
	NewState :: term(), Args1 :: term(), Args2 :: term(),
	Handler2 :: Module2 | {Module2, Id :: term()},
	Module2 :: atom().
handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

-spec handle_info(Info :: term(), State :: term()) -> Result when
	Result :: {ok, NewState}
			| {ok, NewState, hibernate}
			| {swap_handler, Args1, NewState, Handler2, Args2}
			| remove_handler,
	NewState :: term(), Args1 :: term(), Args2 :: term(),
	Handler2 :: Module2 | {Module2, Id :: term()},
	Module2 :: atom().
handle_info(Info, State) ->
    {ok, State}.

-spec terminate(Arg, State :: term()) -> term() when
	Arg :: Args
		| {stop, Reason}
		| stop
		| remove_handler
		| {error, {'EXIT', Reason}}
		| {error, Term :: term()},
	Args :: term(), Reason :: term().
terminate(Arg, State) ->
    ok.

-spec code_change(OldVsn, State :: term(), Extra :: term()) -> {ok, NewState :: term()} when
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
code_change(OldVsn, State, Extra) ->
    {ok, State}.
