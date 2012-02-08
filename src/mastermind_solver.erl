-module(mastermind_solver).

-compile(export_all).

first(BoardSize) -> lists:seq(0, BoardSize-1).

next(Board, NumberOfColors) -> next(Board, lists:seq(0, NumberOfColors-1), length(Board)).

next([], _Available, 0) -> none;
next([H|T], Available, Depth) ->
	case next(T, lists:delete(H, Available), Depth-1) of
		none -> 
			case next_available(H, Available) of
				none -> none;
				H1 -> [ H1|nthhead(Depth-1, lists:delete(H1, Available)) ]
			end;
		Next -> [H|Next]
	end.

next_available(X, [X]) -> none;
next_available(X, [X,Y|_]) -> Y;
next_available(X, [_|T]) -> next_available(X, T).


nthhead(N, L) ->
	{ H, _ } = lists:split(N, L),
	H.

count_blacks_and_whites(Board, Move) ->
        lists:foldl(fun({ A, A }, { B, W }) -> { B+1, W };
                       ({ A, _ }, { B, W }) -> 
                                case lists:member(A, Board) of
                                        true -> { B, W+1 };
                                        false -> { B, W }
                                end
                    end,{ 0, 0 }, lists:zip(Move, Board)).

check_candidate(Candidate, History) ->
	lists:all(fun({ HistoricMove, { Blacks, Whites } }) ->
		case count_blacks_and_whites(Candidate, HistoricMove) of
			{ Blacks, Whites } -> true;
			_ -> false
		end
	end, History).

play(BoardSize, NumberOfColors) ->
	F = mastermind_session_sup:start_session(self(), BoardSize, NumberOfColors),
	play(F, first(BoardSize), BoardSize, NumberOfColors, []).

play(F, Candidate, BoardSize, NumberOfColors, History) ->
	case check_candidate(Candidate, History) of
		false -> play(F, next(Candidate, NumberOfColors), BoardSize, NumberOfColors, History);
		true -> 
			case F(Candidate) of
				{ done, N } -> 
					io:format("~p: Solved in ~b moves!~n", [ Candidate, N ]);
				{ accepted, BlacksAndWhites } ->
					io:format("~p: ~p~n", [ Candidate, BlacksAndWhites ]),
					play(F, next(Candidate, NumberOfColors), BoardSize, NumberOfColors, [ { Candidate, BlacksAndWhites }|History ])
			end
	end.
