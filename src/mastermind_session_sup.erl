-module(mastermind_session_sup).

-behaviour(supervisor).

-export([ start_link/0, start_session/3 ]).
-export([ init/1 ]).

start_link() ->
	supervisor:start_link({ local, ?MODULE }, ?MODULE, void).

init(void) ->
	{ ok, {
		{ one_for_one, 2, 5 },
		[]
	} }.

start_session(ClientPid, BoardSize, NumberOfColors) when is_pid(ClientPid), 
                                                         NumberOfColor >= BoardSize, 
                                                         BoardSize <= 100, NumberOfColors <= 100 ->
	{ ok, Pid } =	
		supervisor:start_child(?MODULE, {
			{ mastermind_session, ClientPid }, 
			{ mastermind_session, start_link, [ ClientPid, BoardSize, NumberOfColors ] },
			temporary, brutal_kill, worker, [ mastermind_session ]
		}),
	fun(Move) ->
		mastermind_session:move(Pid, Move)
	end.
