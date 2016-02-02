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
	phrase(spec_cons(Cons), Spec, []),
	!.

solve(Spec, Ans) :-
	phrase(spec_cons(Cons), Spec, []),
	!,
	nonogram(Cons, Matrix),
	join(Matrix, Ans, 10).


%% nonogram(?Constraints, ?Matrix)
% True when Matrix is a nonogram given by the constraints.

nonogram(cons(Cols, Rows), Grid) :-
	( nonvar(Grid) ->
		matrix(N, M, Grid),
		length(Rows, N),
		length(Cols, M)
	;
		length(Cols, M),
		length(Rows, N),
		matrix(N, M, Grid)
	),
	nonogram_(Rows, Grid),
	transpose(Grid, Trans),
	nonogram_(Cols, Trans),
	flatten(Grid, Vars),
	label(Vars).

nonogram_([], []).
nonogram_([Spec|T], [Row|Grid]) :-
	nonvar(Spec), !,
	findall(Row, phrase(con_codes(Spec), Row, []), Rows),
	tuples_in([Row], Rows),
	nonogram_(T, Grid).
nonogram_([Spec|T], [Row|Grid]) :-
	nonvar(Row), !,
	phrase(con_codes(Spec), Row, []),
	nonogram_(T, Grid).


%% spec_cons(?Constraints)//
% Grammar to parse/generate constraint specs.

spec_cons(cons(Cols, Rows)) -->
	spec_cons__cols(Cols),
	spec_cons__rows(Rows).

spec_cons__constraint([run(Char,Len)|T], T) -->
	"(",
	[Char],
	",",
	integer(Len),
	")".
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


%% con_codes(?Constraints)//
% Grammar to parse/generate vectors adheraring to nonogram constraints.

con_codes([]) --> [].
con_codes([run(A,ALen),run(B,BLen)|T]) -->
	{ A #\= B },
	{ A #\= 32, B #\= 32 },
	con_codes_(run(A,ALen), 0),
	con_codes([run(B,BLen)|T]).
con_codes([run(Char,ALen),run(Char,BLen)|T]) -->
	{ Char #\= 32 },
	con_codes_(run(Char,ALen), 0),
	" ",
	con_codes([run(Char,BLen)|T]).
con_codes([run(Char,Len)]) -->
	{ Char #\= 32 },
	con_codes_(run(Char,Len), 0),
	con_codes([]).
con_codes(Row) --> " ", con_codes(Row).

con_codes_(run(Char,Len), Count) --> [Char],
	{
		Count #< Len,
		Count1 #= Count + 1
	},
	con_codes_(run(Char,Len), Count1).
con_codes_(run(_,Len), Len) --> { Len #\= 0 }.


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

matrix(N, M, Grid) :-
	length(Grid, N),
	matrix_(M, Grid).
	matrix_(_, []).
matrix_(M, [H|T]) :-
	length(H, M),
	matrix_(M, T).


%% join(?Lines, ?Str, ?Char)
% Str is the concatination of the Lines separated by Char.

join([], [], _).
join([[]|Lines], [Char|Ans], Char) :- join(Lines, Ans, Char).
join([[H|T]|Lines], [H|Ans], Char) :- join([T|Lines], Ans, Char).
