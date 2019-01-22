#!/usr/bin/env swipl

:- initialization(main, main).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).


% --------------------------------------------------
% GENERAL HELPERS

%% domain(Vs, Min, Max)
% The lowest bound among finite domain variable Vs is Min.
% The highest bound among finite domain variable Vs is Max.
domain([V], Min, Max) :- !, fd_inf(V, Min), fd_sup(V, Max).
domain([H|T], Min, Max) :-
	domain(T, Min0, Max0),
	fd_inf(H, Min1),
	fd_sup(H, Max1),
	Min #= min(Min0, Min1),
	Max #= max(Max0, Max1).


%% cardinality(+Vs, -Cardinality)
% Cardinality is a list of Key-Num pairs, where Key is in the domain of the
% finite domain variables Vs and Num is its number of occurences. This is
% like the CLP(FD) builtin constraint `global_cardinality`, but Cardinality
% is allowed to be unbound.
cardinality(Vs, Cardinality) :-
	domain(Vs, Min, Max),
	label([Min, Max]),
	findall(K, between(Min, Max, K), Keys),
	pairs_keys_values(Cardinality, Keys, _),
	global_cardinality(Vs, Cardinality).


% --------------------------------------------------
% BOARD

%% board(Shape, Board)
%% board(Shape, Board, Vals)
% Board is a 2D board of the given Shape. Vals is a 1D list of values
% distributed on the board left-to-right, top-to-bottom.
board(Shape, Board) :-
	board(Shape, Board, _Vals).

board(Shape, Board, Vals) :-
	Board = board(Shape, Vals),
	Shape = [H, W],
	Length #= H*W,
	length(Vals, Length),
	Vals ins 1..Length.


%% board_element(Board, Element, Coords)
% Coords are the coordinates of Element on a Board.
board_element(Board, Element, Coords) :-
	Board = board(Shape, Values),
	Shape = [H, W],
	Coords = [Y, X],
	Y in 1..H,
	X in 1..W,
	I #= (Y-1)*W + (X-1)*1 + 1,
	element(I, Values, Element).


%% board_grid(Board, Grid)
% Grid is a 2D list representation of the Board.
board_grid(Board, Grid) :-
	Board = board(Shape, Vals),
	Shape = [H, W],
	length(Grid, H),
	maplist({W}/[Row] >> length(Row, W), Grid),
	append(Grid, Vals).


% --------------------------------------------------
% SUDOKU

%% sudoku(Board)
% Board is a 9x9 board whose values constitute a valid standard sudoku.
sudoku(Board) :-
	Rows = [
		[A1, A2, A3, A4, A5, A6, A7, A8, A9],
		[B1, B2, B3, B4, B5, B6, B7, B8, B9],
		[C1, C2, C3, C4, C5, C6, C7, C8, C9],
		[D1, D2, D3, D4, D5, D6, D7, D8, D9],
		[E1, E2, E3, E4, E5, E6, E7, E8, E9],
		[F1, F2, F3, F4, F5, F6, F7, F8, F9],
		[G1, G2, G3, G4, G5, G6, G7, G8, G9],
		[H1, H2, H3, H4, H5, H6, H7, H8, H9],
		[I1, I2, I3, I4, I5, I6, I7, I8, I9]
	],

	Cols = [
		[A1, B1, C1, D1, E1, F1, G1, H1, I1],
		[A2, B2, C2, D2, E2, F2, G2, H2, I2],
		[A3, B3, C3, D3, E3, F3, G3, H3, I3],
		[A4, B4, C4, D4, E4, F4, G4, H4, I4],
		[A5, B5, C5, D5, E5, F5, G5, H5, I5],
		[A6, B6, C6, D6, E6, F6, G6, H6, I6],
		[A7, B7, C7, D7, E7, F7, G7, H7, I7],
		[A8, B8, C8, D8, E8, F8, G8, H8, I8],
		[A9, B9, C9, D9, E9, F9, G9, H9, I9]
	],

	Blocks = [
		[A1, A2, A3, B1, B2, B3, C1, C2, C3],  % top-left
		[A4, A5, A6, B4, B5, B6, C4, C5, C6],  % top-center
		[A7, A8, A9, B7, B8, B9, C7, C8, C9],  % top-right
		[D1, D2, D3, E1, E2, E3, F1, F2, F3],  % center-left
		[D4, D5, D6, E4, E5, E6, F4, F5, F6],  % true center
		[D7, D8, D9, F7, F8, F9, E7, E8, E9],  % center-right
		[G1, G2, G3, H1, H2, H3, I1, I2, I3],  % bottom-left
		[G4, G5, G6, H4, H5, H6, I4, I5, I6],  % bottom-center
		[G7, G8, G9, I7, I8, I9, H7, H8, H9]   % bottom-right
	],

	append(Rows, Vals),
	Vals ins 1..9,

	maplist(all_distinct, Rows),
	maplist(all_distinct, Cols),
	maplist(all_distinct, Blocks),
	global_cardinality(Vals, [1-9, 2-9, 3-9, 4-9, 5-9, 6-9, 7-9, 8-9, 9-9]),

	Shape = [9, 9],
	board(Shape, Board, Vals).


% --------------------------------------------------
% TOUR

%% tour(Shape, Tour)
% Tour is a tour of a chess board of the given Shape.
% Moves from any cell to any other cell are allowed.
%
% Tour is a term, tour(Shape, Nodes), where Nodes is the list of nodes
% in visitation order. Nodes are natural numbers between 1 and N where N
% is the number of nodes.
tour(Shape, Tour) :-
	Tour = tour(Shape, Nodes),
	Shape = [H, W],
	Length #= H*W,
	length(Nodes, Length),
	Nodes ins 1..Length,
	all_distinct(Nodes).


%% tour_node(Tour, Node, Coords)
% Coords is the coordinates of a Node belonging to a Tour.
tour_node(Tour, Node, Coords) :-
	Tour = tour(Shape, _Nodes),
	Shape = [H, W],
	Coords = [Y, X],
	Y in 1..H,
	X in 1..W,
	Node #= (Y-1)*W + (X-1)*1 + 1.


%% tour_coords(Tour, Coords)
% Coords is the list of coordinates of nodes in the Tour in visitation order.
tour_coords(Tour, Coords) :-
	Tour = tour(_Shape, Nodes),
	same_length(Nodes, Coords),
	maplist(tour_node(Tour), Nodes, Coords).


%% tour_grid(Tour, Grid)
% Grid is a 2D list representation of the Tour.
tour_grid(Tour, Grid) :-
	Tour = tour(Shape, Nodes),
	Shape = [H, W],
	length(Grid, H),
	maplist({W}/[Row] >> length(Row, W), Grid),
	append(Grid, Flat),
	tour_grid_(Nodes, Nodes, Flat).

tour_grid_([], _AllNodes, _Flat).
tour_grid_([N|Nodes], AllNodes, Flat) :-
	element(O, AllNodes, N),
	element(N, Flat, O),
	tour_grid_(Nodes, AllNodes, Flat).


%% tour_vals(Tour, Board, TourVals)
% TourVals are the values on the Board in the order visited by Tour.
tour_vals(Tour, Board, TourVals) :-
	Tour = tour(Shape, Nodes),
	Board = board(Shape, Vals),
	length(Nodes, Size),
	length(Vals, Size),
	length(TourVals, Size),
	domain(Vals, Min, Max),
	TourVals ins Min..Max,
	cardinality(Vals, Cardinality),
	cardinality(TourVals, Cardinality),
	tour_vals_(Nodes, TourVals, Tour, Board).

tour_vals_([], [], _Tour, _Board).
tour_vals_([N|Nodes], [V|TourVals], Tour, Board) :-
	tour_node(Tour, N, Coords),
	board_element(Board, V, Coords),
	tour_vals_(Nodes, TourVals, Tour, Board).


% --------------------------------------------------
% KNIGHTS TOUR

%% knights_tour(Shape, Tour)
% Tour is a knights tour of a chess board of the given Shape.
% Only valid knight moves are allowed.
%
% See tour/2.
knights_tour(Shape, Tour) :-
	knights_tour(Shape, Tour, _Nodes).

knights_tour(Shape, Tour, Nodes) :-
	tour(Shape, Tour, Nodes),
	Shape = [H, W],
	knights_tour_(Nodes, Tour, H, W).


knights_tour_([_LastNode], _Tour, _H, _W) :- !.
knights_tour_([Prev, Next|T], Tour, H, W) :-
	[Y0, Y1] ins 1..H,
	[X0, X1] ins 1..W,
	[DY, DX] ins -2 \/ -1 \/ +1 \/ +2,
	DY #= Y0 - Y1,
	DX #= X0 - X1,
	abs(DY) + abs(DX) #= 3,
	tour_node(Tour, Prev, [Y0, X0]),
	tour_node(Tour, Next, [Y1, X1]),
	knights_tour_([Next|T], Tour, H, W).


% --------------------------------------------------
% SCORE

%% score(Tour, Board, Digits, Score)
% The Digits of Score are taken from the Board in the order visited by Tour.
score(Tour, Board, Digits, Score) :-
	tour_vals(Tour, Board, Digits),
	digits_score(Digits, Score).


%% digits_score(Digits, Score)
% Digits is the list of digits of the integer Score.
digits_score(Digits, Score) :-
	digits_score_(Digits, Score, 0).

digits_score_([], Score, Score) :- !.
digits_score_([D|Digits], Score, Prev) :-
	D in 1..9,
	Next #= 10 * Prev + D,
	digits_score_(Digits, Score, Next).


% --------------------------------------------------
% SOLVERS

%% label_(Opts, TourBoardVars)
%% label_(TourBoardVars)
% A helper to label Tours, Boards, and lists of variables.
% Opts is passed to labeling/2.
label_(Opts, tour(_Shape, Nodes)) :- !, labeling(Opts, Nodes).
label_(Opts, board(_Shape, Vals)) :- !, labeling(Opts, Vals).
label_(Opts, Vars) :- !, labeling(Opts, Vars).
label_(X) :- label_([], X).


%% label_optimal(Tour, Board, Score)
%
label_optimal(Tour, Board, Score) :-
	Tour = tour(_Shape, Nodes),
	score(Tour, Board, Digits, Score),
	label_optimal_(Nodes, Digits).

label_optimal_([], []).
label_optimal_([N|Nodes], [D|Digits]) :-
	labeling([down], [D]),
	labeling([up, max(D)], [N]),
	label_optimal_(Nodes, Digits).


% --------------------------------------------------
% KNOWN SOLUTIONS

%% known_soln(Name, Tour, Board, Score)
% Name identifies a known solution.
% Tour is the 9x9 knights tour of the solution.
% Board is the sudoku board of the solution.
% Score is the score of the Tour on the Board.
known_soln(Name, Tour, Board, Score) :-
	known_soln_(Name, CoordList, Grid, Score),

	tour([9,9], Tour),
	knights_tour([9,9], Tour),
	tour_coords(Tour, CoordList),

	board([9,9], Board),
	sudoku(Board),
	board_grid(Board, Grid),

	score(Tour, Board, _Digits, Score).

known_soln_(
	u_SingularInfinity_1,
	[
		[2, 2], [4, 3], [6, 4], [8, 5], [7, 7], [5, 8], [3, 9], [4, 7], [2, 8],
		[1, 6], [3, 5], [5, 4], [6, 2], [8, 3], [9, 1], [7, 2], [8, 4], [9, 6],
		[8, 8], [6, 9], [5, 7], [3, 8], [4, 6], [6, 5], [8, 6], [9, 8], [7, 9],
		[6, 7], [5, 9], [7, 8], [9, 9], [8, 7], [9, 5], [7, 6], [9, 7], [8, 9],
		[6, 8], [4, 9], [3, 7], [1, 8], [2, 6], [1, 4], [3, 3], [4, 1], [5, 3],
		[4, 5], [6, 6], [7, 4], [9, 3], [8, 1], [7, 3], [9, 2], [7, 1], [5, 2],
		[3, 1], [1, 2], [2, 4], [3, 6], [5, 5], [3, 4], [1, 3], [2, 1], [4, 2],
		[6, 1], [8, 2], [9, 4], [7, 5], [5, 6], [4, 8], [2, 9], [1, 7], [2, 5],
		[4, 4], [6, 3], [5, 1], [3, 2], [1, 1], [2, 3], [1, 5], [2, 7], [1, 9]
	], [
		[8, 3, 6, 4, 7, 9, 1, 2, 5],
		[4, 9, 7, 1, 2, 5, 3, 8, 6],
		[1, 2, 5, 3, 8, 6, 4, 7, 9],
		[6, 5, 9, 2, 3, 7, 8, 4, 1],
		[7, 4, 2, 8, 5, 1, 6, 9, 3],
		[3, 8, 1, 9, 6, 4, 2, 5, 7],
		[5, 7, 3, 6, 4, 2, 9, 1, 8],
		[2, 1, 8, 7, 9, 3, 5, 6, 4],
		[9, 6, 4, 5, 1, 8, 7, 3, 2]
	],
	999999988988889778676776338231251274514254562346423654131653645315414612217287735
).


% --------------------------------------------------
% IO

%% read_board(StreamOrFile, Board, Shape)
% Reads a board from StreamOrFile.
% StreamOrFile may be a stream alias or a file name.
read_board(StreamOrFile, Board, Shape) :-
	is_stream(StreamOrFile), !,
	Stream = StreamOrFile,
	read_board_(Board, Stream, Shape).

read_board(StreamOrFile, Board, Shape) :-
	File = StreamOrFile,
	open(File, read, Stream, []),
	read_board_(Board, Stream, Shape),
	close(Stream).

read_board_(Board, Stream, Shape) :-
	board(Shape, Board, Vals),
	Shape = [H, W],
	phrase_from_stream(board_dcg(H, W, Rows), Stream),
	append(Rows, Vals).

board_dcg(H, _, []) --> eos, {H #= 0, !}.
board_dcg(H, W, [R|Rows]) -->
	row_dcg(W, R),
	board_dcg(H-1, W, Rows).

row_dcg(W, []) --> "\n", {W #= 0, !}.
row_dcg(W, [V|Vals]) -->
	integer(V),
	whites,
	row_dcg(W-1, Vals).


%% print_board(Board)
% Prints the Board to the current output.
print_board(Board) :-
	board_grid(Board, Grid),
	forall(member(Row, Grid), (
		forall(member(V, Row), (
			format('~|~t~w~4+', [V])
		)),
		nl
	)).


%% print_tour(Tour)
% Prints the Tour to the current output.
print_tour(Tour) :-
	tour_grid(Tour, Grid),
	forall(member(Row, Grid), (
		forall(member(V, Row), (
			format('~|~t~w~4+', [V])
		)),
		nl
	)).


%% print(X)
%
print(X) :- print_tour(X), nl, !.
print(X) :- print_board(X), nl, !.
print(X) :- write(X), nl, !.


% --------------------------------------------------
% MAIN

%% main(Options)
% Reads a board from the current input and finds the knights tour that
% maximizes the score.
%
% Options:
%     source(Source):
%         If given, read a board from Source, which may be a file name or stream.
%         Otherwise the board is
%     board(Board):
%         Board is the board of the solution.
%     tour(Tour):
%         Tour is the tour of the solution.
%     score(Score):
%         Score is the score of the solution.
%     prefix(Prefix):
%         Prefix is a prefix of the score.
%         Useful for pruning the search space.
main(Options) :-
	option(source(Source), Options, _),
	option(tour(Tour), Options, _),
	option(board(Board), Options, _),
	option(score(Score), Options, _),
	option(prefix(Prefix), Options, _),

	digits_score(DigitsPrefix, Prefix),
	once(append(DigitsPrefix, _, Digits)),

	(ground(Source) ->
		read_board(Source, Board, Shape),
		knights_tour(Shape, Tour)
	;
		sudoku(Board),
		knights_tour([9,9], Tour)
	),

	score(Tour, Board, Digits, Score),
	label_optimal(Tour, Board, Score),

	print('Board:'), print(Board),
	print('Tour:'), print(Tour),
	print('Score:'), print(Score),

	true.
