#!/usr/bin/env swipl -g main -s

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).


%% main
% This program solves ascii nonograms given constraint specs and generates
% constraint specs given ascii nonograms. It expects exactly one command line
% argument, `solve` or `generate`, to determine the mode of operation.

main :-
	prompt(_, ''),
	current_prolog_flag(argv, [solve]),
	read_stream_to_codes(user_input, Spec),
	solve(Spec, Ans),
	writef("%s", [Ans]),
	halt.
main:-
	prompt(_, ''),
	current_prolog_flag(argv, [generate]),
	read_stream_to_codes(user_input, Ans),
	generate(Spec, Ans),
	writef("%s", [Spec]),
	halt.

generate(Spec, Ans) :-
	join(Matrix, Ans, 10),
	nonogram(Cons, Matrix),
	phrase(spec_cons(Cons), Spec),
	!.

solve(Spec, Ans) :-
	phrase(spec_cons(Cons), Spec),
	!,
	nonogram(Cons, Matrix),
	join(Matrix, Ans, 10).


%% nonogram(?Constraints, ?Matrix)
% True when Matrix is a nonogram given by the constraints.

nonogram(cons(Cols, Rows), Grid) :-
	matrix(N, M, Grid),
	length(Cols, M),
	length(Rows, N),
	nonogram_(Rows, Grid),
	transpose(Grid, Trans),
	nonogram_(Cols, Trans),
	flatten(Grid, Vars),
	label(Vars).

nonogram_([], []).
nonogram_([Runs|Next], [Row|Rows]) :-
	Row = [X|_],
	(nonvar(X) ->
		phrase(row(Runs), Row)
	;
		runs_arcs(Runs, Arcs),
		automaton(Row, [source(start),sink((1,0))], Arcs)
	),
	nonogram_(Next, Rows).


%% runs_arcs(+Runs, -Arcs)
% Construct the arcs of an automaton that accepts the rows described by the
% constraint runs. The start state is `start` and the accept state is `(1,0)`.

runs_arcs(Runs, Arcs) :-
	Runs = [run(X,Len)|_],
	Len0 #= Len - 1,
	length(Runs, N),
	Arcs = [arc(start,32,start),arc(start,X,(N,Len0))|Rest],
	runs_arcs(Runs, Rest, N).

runs_arcs([run(A,ALen),run(B,BLen)|Runs], Arcs, N) :-
	A #\= B,
	BLen0 #= BLen - 1,
	N0 #= N - 1,
	findall(Arc, (
		Arc = arc((N,Count),A,(N,Count0)),
		between(1,ALen,Count),
		Count0 #= Count - 1
	;
		member(Arc, [
			arc((N,0),32,(N,0)),
			arc((N,0),B,(N0,BLen0))])
	), HeadArcs),
	append(HeadArcs, TailArcs, Arcs),
	runs_arcs([run(B,BLen)|Runs], TailArcs, N0).

runs_arcs([run(X,ALen),run(X,BLen)|Runs], Arcs, N) :-
	BLen0 #= BLen - 1,
	N0 #= N - 1,
	findall(Arc, (
		Arc = arc((N,Count),X,(N,Count0)),
		between(1,ALen,Count),
		Count0 #= Count - 1
	;
		member(Arc, [
			arc((N,0),32,(N,space)),
			arc((N,space),32,(N,space)),
			arc((N,space),X,(N0,BLen0))])
	), HeadArcs),
	append(HeadArcs, TailArcs, Arcs),
	runs_arcs([run(X,BLen)|Runs], TailArcs, N0).

runs_arcs([run(X,Len)], Arcs, N) :-
	findall(Arc, (
		Arc = arc((N,Count),X,(N,Count0)),
		between(1,Len,Count),
		Count0 #= Count - 1
	;
		Arc = arc((N,0),32,(N,0))
	), Arcs).

run_arcs([], [], _).


%% spec_cons(?Constraints)//
% Grammar to parse/generate constraint specs.

spec_cons(cons(Cols, Rows)) -->
	spec_cons__cols(Cols),
	spec_cons__rows(Rows).

spec_cons__constraint([run(Char,Len)|T], T) -->
	"(", [Char], ",", integer(Len), ")".
spec_cons__constraint(X, X) --> "     ".

spec_cons__cols(Cols) -->
	"Columns:\n",
	spec_cons__cols_(1, Cols).

spec_cons__cols_(1, Cols) --> "\n", { all_empty(Cols) }.
spec_cons__cols_(N, Cols) -->
	{ select_at(N, Cons, Cols, Rest, NewCols) },
	spec_cons__constraint(Cons, Rest),
	(
		" ",
		{ N1 #= N+1 },
		spec_cons__cols_(N1, NewCols)
	|
		"\n",
		spec_cons__cols_(1, NewCols)
	).

spec_cons__rows(Rows) -->
	"Rows:\n",
	spec_cons__rows_(Rows).

spec_cons__rows_([]) --> eos.
spec_cons__rows_([]) --> "\n".
spec_cons__rows_([Con|Rows]) -->
	spec_cons__constraint(Con, T),
	(
		{ T = [] },
		"\n",
		spec_cons__rows_(Rows)
	|
		" ",
		spec_cons__rows_([T|Rows])
	).


%% row(?Runs)//
% Grammar to parse/generate vectors adheraring to nonogram constraints.

row([]) --> [].
row([run(A,ALen),run(B,BLen)|T]) -->
	{ A #\= B },
	{ A #\= 32, B #\= 32 },
	row_(run(A,ALen), 0),
	row([run(B,BLen)|T]).
row([run(Char,ALen),run(Char,BLen)|T]) -->
	{ Char #\= 32 },
	row_(run(Char,ALen), 0),
	" ",
	row([run(Char,BLen)|T]).
row([run(Char,Len)]) -->
	{ Char #\= 32 },
	row_(run(Char,Len), 0),
	row([]).
row(Row) --> " ", row(Row).

row_(run(Char,Len), Count) --> [Char],
	{
		Count #< Len,
		Count1 #= Count + 1
	},
	row_(run(Char,Len), Count1).
row_(run(_,Len), Len) --> { Len #\= 0 }.


%% select_at(+N, ?X, ?XList, ?Y, ?YList)
% True when the Nth value of XList is X and the Nth value of YList is Y.
% XList and YList are otherwise the same.

select_at(1, X, [X|List], Y, [Y|List]).
select_at(N, X, [H|XList], Y, [H|YList]) :-
	N > 0,
	N0 is N - 1,
	select_at(N0, X, XList, Y, YList).


%% all_empty(?Lists)
% True when Lists is a list of empty lists.

all_empty([]).
all_empty([[]|T]) :- all_empty(T).


%% matrix(?N, ?M, ?Matrix)
% True when Matrix is an N by M matrix.

matrix(N, M, [H|T]) :-
	bfs([N, M]),
	length([H|T], N),
	length(H, M),
	maplist(same_length(H), T).


%% bfs(+Vars)
%% bfs(+Vars, +Max)
% Searches integer variables in a breadth-first order.
%
% Vars is a list of integers between Min and Max. The values are initially
% unified to 0 and increase upon backtracking such that every combination is
% searched. The shorthand `bfs(Vars)` is equivalent to `bfs(Vars, 0, inf)`.

bfs(Vars) :- bfs(Vars, 0, inf).
bfs(Vars, Min, Max) :-
	between(Min, Max, X),
	(
		maplist(=(X), Vars)
	;
		member(X, Vars),
		maplist(between(Min, X), Vars),
		\+ maplist(=(X), Vars)
	).


%% join(?Lines, ?Str, ?Char)
% Str is the concatination of the Lines separated by Char.

join([], [], _).
join([[]|Lines], [Char|Ans], Char) :- join(Lines, Ans, Char).
join([[H|T]|Lines], [H|Ans], Char) :- join([T|Lines], Ans, Char).
