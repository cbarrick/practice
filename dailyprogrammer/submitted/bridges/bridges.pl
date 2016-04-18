#!/usr/bin/env swipl -q -g main -t halt -s
:- use_module(library(clpfd)).

%% main
% Calls solve/2 with the islands from the current input and prints the bridges.
main :-
	read_islands(Islands),
	solve(Islands, Bridges),
	write_bridges(Bridges).

%% read_islands(-Islands)
% Reads the input.
read_islands(Islands) :-
	read(X),
	(X = end_of_file ->
		Islands = []
	; X = island(_,_,_) ->
		Islands = [X|T],
		read_islands(T)
	).

%% write_bridges(+Bridges)
% Prints the output.
write_bridges(Bridges) :- write_bridges_(Bridges, 0).

write_bridges_([], _) :- !.
write_bridges_([Row|Matrix], I) :-
	length(Exclude, I),
	append(Exclude, Include, Row),
	write_bridges__(Include, I, I),
	I1 is I+1,
	write_bridges_(Matrix, I1).

write_bridges__([], _, _) :- !.
write_bridges__([0|T], I, J) :- !,
	J1 is J + 1,
	write_bridges__(T, I, J1).
write_bridges__([N|T], I, J) :-
	format("bridge(~w,~w).\n", [I, J]),
	N0 is N - 1,
	write_bridges__([N0|T], I, J).

%% solve(+Islands, -Bridges)
% Solves a bridges puzzle. A bridges puzzle is played on a 2D grid. Some cells
% on this grid are called "islands". Islands are connected by "bridges" which
% run either horizontal or vertical on the grid. Each island is reachable from
% any other island over these bridges, and each island has a fixed degree, the
% number of bridges connected to it. Bridges may not cross islands or other
% bridges, but multiple bridges may connect the same islands. At most two
% bridges may connect the same pair of islands.
%
% Arguments:
% - Islands is a list of island constraints giving the degree and coordinates
%   of each island, in the form of `island(Degree, X, Y)`.
% - Bridges is an adjacency matrix of integers from 0 to 8 giving the number of
%   bridges connecting the islands.
solve(Islands, Bridges) :-
	length(Islands, N),
	square_matrix(N, Bridges),
	fix_degree(Islands, Bridges),
	fix_neighbors(Islands, Bridges),
	fix_overlap(Islands, Bridges),
	label_matrix(Bridges),
	connected(Bridges).

label_matrix([]) :- !.
label_matrix([H|T]) :- label(H), label_matrix(T).

%% square_matrix(+N, -Matrix)
% Matrix is an NxN matrix.
square_matrix(N, Matrix) :-
	length(Matrix, N),
	square_matrix_(Matrix, N).

square_matrix_([], _) :- !.
square_matrix_([H|T], N) :-
	length(H, N),
	square_matrix_(T, N).

%% fix_degree(+Islands, ?Bridges)
% Bounds the Bridges matrix according to the degree of the islands.
fix_degree(Islands, Bridges) :-
	transpose(Bridges, Bridges),
	fix_degree_(Islands, Bridges).

fix_degree_([], _) :- !.
fix_degree_([island(_,_,D)|Islands], [Row|Bridges]) :-
	Row ins 0..2,
	sum(Row, #=, D),
	fix_degree_(Islands, Bridges).

%% fix_neighbors(+Islands, ?Bridges)
% Ensures that bridges only connect neighboring islands.
fix_neighbors(Islands, Bridges) :-
	length(Islands, L),
	findall([I,J], (between(1,L,I), between(1,L,J)), Pairs),
	maplist(fix_neighbors_(Islands, Bridges), Pairs).

fix_neighbors_(Islands, _, [I, J]) :- neighboring(Islands, I, J), !.
fix_neighbors_(_, Bridges, [I, J]) :- matrix_at(I, J, Bridges, 0).

%% fix_overlap(+Islands, ?Bridges)
% Ensures that bridges do not cross each other.
fix_overlap(Islands, Bridges) :-
	length(Islands, L),
	findall(pair(bridge(I, J), bridge(H, K)), (
		between(1,L,I),
		neighboring(Islands, I, J),
		I < J,
		between(I,L,H),
		H \= I,
		neighboring(Islands, H, K),
		H < K
	), Pairs),
	maplist(fix_overlap_(Islands, Bridges), Pairs).

fix_overlap_(Islands, Bridges, pair(bridge(I, J), bridge(H, K))) :-
	nth1(I, Islands, island(IX, IY, _)),
	nth1(J, Islands, island(JX, JY, _)),
	nth1(H, Islands, island(HX, HY, _)),
	nth1(K, Islands, island(KX, KY, _)),
	(IX = JX, HY = KY ->
		R #<== (IY #< HY #/\ HY #< JY) #/\ (HX #< IX #/\ IX #< KX)
	; IY = JY, HX = KX ->
		R #<== (HY #< IY #/\ IY #< KY) #/\ (IX #< HX #/\ HX #< JX)
	;
		R = 0
	),
	matrix_at(I, J, Bridges, IJ),
	matrix_at(H, K, Bridges, HK),
	R #==> (IJ #= 0 #\/ HK #= 0).

%% connected(+Matrix)
% Matrix is an adjacency matrix of a connected graph.
% I wish I could figure out how to do this with CLP(FD).
connected(Matrix) :-
	length(Matrix, Order),
	findall(I, between(2,Order,I), Is),
	connected_(Matrix, Is, [1]).

connected_(_, [], _) :- !.
connected_(Matrix, Unknown, Known) :-
	select(I, Unknown, Rest),
	nth1(I, Matrix, Row),
	nth1(J, Row, X),
	X > 0,
	member(J, Known),
	!,
	connected_(Matrix, Rest, [I|Known]).

%% neighboring(+Islands, ?I, ?J)
% True when the islands at indicies I and J are neighbors in the X or Y
% direction. Bridges may only connect neighbors.
neighboring(Islands, I, J) :-
	nth1(I, Islands, island(IX, IY, _)),
	nth1(J, Islands, island(JX, JY, _)),
	J \= I,
	(JX = IX ; JY = IY),
	\+ (
		nth1(K, Islands, island(KX, KY, _)),
		K \= I,
		K \= J,
		(JX = IX ->
			KX = IX,
			(IY < JY -> IY < KY ; KY < IY),
			abs(IY - KY) < abs(IY - JY)
		;
			KY = IY,
			(IX < JX -> IX < KX ; KX < IX),
			abs(IX - KX) < abs(IX - JX)
		)
	).

%% matrix_at(?I, ?J, ?Matrix, ?Elm)
% True when Elm is the element of Matrix at row I, column J.
matrix_at(I, J, Matrix, Elm) :-
	nth1(I, Matrix, Row),
	nth1(J, Row, Elm).
