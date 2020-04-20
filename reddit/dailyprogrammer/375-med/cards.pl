% Naive solution
% ---------------------------------------------------------------------------

%% flip(Before, After)
% The transition function between card states.
%
% - A face up card (`1`) becomes face down (`0`).
% - A face down card (`0`) becomes face up (`1`).
% - A card that has been taken (`2`) remains taken.
flip(0, 1).
flip(1, 0).
flip(2, 2).


%% take(N, Before, After)
% Taking the card at position `N` from the board state `Before` results in
% the board state `After`.
take(1, [1],       [2]).
take(1, [1,A|T],   [2,Z|T])   :- flip(A, Z).
take(2, [A,1],     [Z,2])     :- flip(A, Z).
take(2, [A,1,B|T], [Z,2,Y|T]) :- flip(A, Z), flip(B, Y).

take(N, [H|Before], [H|After]) :-
	integer(N),
	2 < N,
	N0 is N - 1,
	once(take(N0, Before, After)).

take(N, Before, After) :-
	var(N),
	length(Before, L),
	length(After, L),
	between(1, L, N),
	once(take(N, Before, After)).


%% cant_solve(Cards)
% True when `Cards` is unsolvable.
cant_solve(Cards) :- cant_solve_leading_(Cards).
cant_solve(Cards) :- cant_solve_trailing_(Cards).

cant_solve_leading_([0,2|_]).
cant_solve_leading_([0|T]) :- cant_solve_leading_(T).

cant_solve_trailing_([2,0|T]) :- maplist(=(0), T).
cant_solve_trailing_([_|T]) :- cant_solve_trailing_(T).


%% done(Cards).
% True when all cards have been taken.
done([]).
done([2|T]) :- done(T).


%% solve(Cards, Soln).
% Solve the game with an initial state of `Cards`.
solve(Cards, Soln) :-
	length(Cards, M),
	length(Soln, M),
	solve(M, Cards, Soln).

%% solve(C, Cards, Soln).
% Solve the game with a state of `Cards` in `M` moves.
% Unlike `solve/2`, `Cards` may be an intermediate state.
solve(0, Cards, []) :- done(Cards).
solve(M, Cards, [N|Soln]) :-
	\+ cant_solve(Cards),
	0 < M,
	M0 is M - 1,
	take(N, Cards, Next),
	solve(M0, Next, Soln).


% Constraint solver
% ---------------------------------------------------------------------------

:- use_module(library(clpfd)).


%% flip_clp(Before, After)
%
flip_clp(Before, After) :-
	[Before, After] ins 0..2,
	Before #= 0 #<==> After #= 1,
	Before #= 1 #<==> After #= 0,
	Before #= 2 #<==> After #= 2.


%% take_clp(N, Before, After)
%
take_clp(N, Before, After) :-
	length(Before, L),
	length(After, L),
	After ins 0..2,
	Left #= N - 1,
	Right #= N + 1,
	element(N, Before, 1),
	element(N, After, 2),
	once(take_clp_(1, N, Left, Right, Before, After)).

take_clp_(_, _, _, _, [], []).
take_clp_(I, N, Left, Right, [B|Before], [A|After]) :-
	flip_clp(B, F),
	A in 0..2,
	(I #= N #==> A #= 2),
	(I #= Left #==> A #= F),
	(I #= Right #==> A #= F),
	(I #\= N #/\ I #\= Left #/\ I #\= Right #==> A #= B),
	I1 is I + 1,
	take_clp_(I1, N, Left, Right, Before, After).


%% solve_clp
%
solve_clp(Cards, Soln) :-
	length(Cards, L),
	Cards ins 0..1,
	length(Soln, L),
	Soln ins 1..L,
	all_distinct(Soln),
	solve_clp_(Cards, Soln),
	labeling([], Soln).

solve_clp_(Cards, []) :- Cards ins 2.
solve_clp_(Cards, [S|Soln]) :-
	take_clp(S, Cards, Next),
	solve_clp_(Next, Soln).



% Top-level
% ---------------------------------------------------------------------------

%% challenge(Name, Cards).
% The challenge inputs.
challenge(1, [0,1,0,0,1,1,0]).
challenge(2, [0,0,1,0,1,1,0,1,1,1,0,1,0,0,1,0,0,1,0,0,0]).
challenge(3, [1,0,1,0,0,1,0,1,0,1,0,0,1,0,1,1,0,1,1,0,0,1,0,1,1,1,0,1,1,1,1]).
challenge(4, [1,1,0,1,1,1,0,1,1,0,0,0,0,0,0,1,0,1,0,1,1,1,0,1,1,1,0,0,1,1,0]).
challenge(bonus, [
	0,1,0,1,1,1,1,1,1,1,1,1,1,0,0,1,0,0,1,0,1,0,0,0,1,0,0,1,1,0,1,1,
	1,0,0,0,1,0,1,1,1,1,0,0,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0
]).


%% run(Name)
% Execute the challenge with the given `Name` and print the results.
run(Name) :-
	challenge(Name, Cards),
	once(time(solve_clp(Cards, Soln))),  % solve with the clp predicates
	once(solve(Cards, Soln)),  % verify with the naive predicates
	format("challenge = ~w\n", [Name]),
	format("input = ~w\n", [Cards]),
	format("solution = ~w\n", [Soln]).
