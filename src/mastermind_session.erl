-module(mastermind_session).

-behaviour(gen_server).

-export([ start_link/3, move/2 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-record(state, {
	board,
	board_size,
	number_of_colors,
	move_count,
	client_pid
}).

start_link(ClientPid, BoardSize, NumberOfColors) ->
	gen_server:start_link(?MODULE, { ClientPid, BoardSize, NumberOfColors }, []).

move(Pid, Move) ->
	gen_server:call(Pid, { move, Move }).

init({ ClientPid, BoardSize, NumberOfColors }) ->
	erlang:process_flag(trap_exit, true),
	erlang:link(ClientPid),
	{ ok, #state{
		board = generate_board(BoardSize, NumberOfColors),
		board_size = BoardSize,
		number_of_colors = NumberOfColors,
		move_count = 0,
		client_pid = ClientPid
	} }.

handle_call({ move, Move }, _, #state{ board_size=BoardSize }=State) ->
	case check_move(Move, State) of
		false -> 	
			{ stop, bad_move, State };
		true ->
			case count_blacks_and_whites(State#state.board, Move) of
				{ BoardSize, 0 } -> 
					unlink(State#state.client_pid),
					{ stop, normal, { done, State#state.move_count+1 }, incr_move_count(State) };

				BlacksAndWhites -> 
					{ reply, { accepted, BlacksAndWhites }, incr_move_count(State) }

			end
	end.

handle_cast(_, State) -> { noreply, State }.

handle_info(_, State) -> { noreply, State }.

terminate(_, _) -> ok.

code_change(_, S, _) -> { ok, S }.

incr_move_count(#state{ move_count=C }=S) ->
	S#state{ move_count=C+1 }.

check_move(Move, #state{ board_size=BoardSize }) when length(Move) =/= BoardSize -> false;
check_move(Move, #state{ number_of_colors=NumberOfColors }) ->
	case lists:all(fun(C) -> C >= NumberOfColors end, Move) of
		true -> false;
		false ->
			sets:size(sets:from_list(Move)) =:= length(Move)
	end.

count_blacks_and_whites(Board, Move) ->
	lists:foldl(fun({ A, A }, { B, W }) -> { B+1, W };
                       ({ A, _ }, { B, W }) -> 
				case lists:member(A, Board) of
					true -> { B, W+1 };
					false -> { B, W }
				end
		    end,{ 0, 0 }, lists:zip(Move, Board)).

generate_board(BoardSize, NumberOfColors) ->
	{ Board, _ } = lists:split(BoardSize, lists:foldl(fun(_, B) ->
		{ B1, B2 } = lists:split(crypto:rand_uniform(0, NumberOfColors), B),
		B2 ++ lists:reverse(B1)
	end, lists:seq(0, NumberOfColors-1), lists:seq(0, NumberOfColors * 100))),
	Board. 
