#!/usr/bin/env swipl -q -g main -t halt -s

% Solution to a challenge posted to r/dailyprogrammer
%
% Given a list of moves for connect four, identify the first winning move.
%
% This program solves the problem as a constraint satisfaction problem using
% reification.
%
% I create a 6x7 grid of constraint satisfaction variables in the domain {0,1}.
% 0 means player X played in that cell, and likewise 1 means player O played in
% that cell. If the cell is variable, it has not yet been played. Then I create
% a new constraint satisfaction variable called `Winner`, also in the domain
% {0,1}. I constrain Winner such that having four of a kind in a row, column,
% or diagonal forces it to become that kind. Finally I apply moves until Winner
% is forced to take a specific value.
%
% It's not necessarily the fastest way to solve this problem. But if I wanted
% to create an AI to play connect four, this kind of reasoning would be super
% useful.
%
% https://www.reddit.com/r/dailyprogrammer/comments/3fva66

:- use_module(library(clpfd)).

% Challenge input
main :- game([4-4,4-3,3-3,3-3,7-6,6-4,6-6,4-6,1-1,5-2,5-5,2-7,7-7,2-1]).


%! game(+Turns)
% Plays a list of moves. Turns is a list of pairs, X-O, where X is the column
% player 1 plays for that turn and O is the column player 2 plays for that turn.
% We print the winner once it is known.
game(Turns) :-
	new_state(State),
	winner(State, Winner),
	game_(State, Turns, 1, Winner).

game_(State, [X-O|Turns], Turn, Winner) :-
	play(State, 0, X),
	(nonvar(Winner) ->
		format("X won at move ~w\n", [Turn]),
		write_state(State)
	;
		play(State, 1, O),
		(nonvar(Winner) ->
			format("O won at move ~w\n", [Turn]),
			write_state(State)
		;
			Next is Turn+1,
			game_(State, Turns, Next, Winner))).


%! play(+State, +Player, +Column, -Row)
% Player plays in the given Column. The piece falls to a specific Row.
% This binds the first variable cell of the indicated column to Player.
play(State, Player, ColNum) :-
	nth1(ColNum, State, Col),
	play_(Col, Player).

play_([Cell|_], Player) :- var(Cell), !, Cell = Player.
play_([_|Col], Player) :- play_(Col, Player).


%! winner(@State, -Winner)
% Create a new constraint variable, Winner, that is 0 or 1 depending on which
% player wins.
winner(Cols, Winner) :-
	Winner in 0..1,
	transpose(Cols, Rows),
	matrix_diagonals(Cols, Diags),
	constrain_lists(Cols, Winner),
	constrain_lists(Rows, Winner),
	constrain_lists(Diags, Winner),
	!.

constrain_lists([], _).
constrain_lists([[]|Lists], Winner) :- constrain_lists(Lists, Winner).
constrain_lists([[_]|Lists], Winner) :- constrain_lists(Lists, Winner).
constrain_lists([[_,_]|Lists], Winner) :- constrain_lists(Lists, Winner).
constrain_lists([[_,_,_]|Lists], Winner) :- constrain_lists(Lists, Winner).
constrain_lists([[A,B,C,D|Tail]|Lists], Winner) :-
	(A #/\ B #/\ C #/\ D) #==> Winner,
	#\(A #\/ B #\/ C #\/ D) #==> (#\Winner),
	constrain_lists([[B,C,D|Tail]|Lists], Winner).


%! matrix_diagonals(+Matrix, -Diags)
% Given a matrix, find all of its diagonals
matrix_diagonals(Matrix, Diags) :-
	Matrix = [FirstCol|_],
	length(Matrix, NumCols),
	length(FirstCol, NumRows),

	PosSum #= NumCols + NumRows,
	bagof(Diag, Target^(
		between(2, PosSum, Target),
		bagof(Cell, N^M^Col^(
			nth1(N, Matrix, Col),
			nth1(M, Col, Cell),
			Target is N + M
		), Diag)
	), PosDiags),

	NegMin #= -(max(NumCols, NumRows) - 1),
	NegMax #= max(NumCols, NumRows) - 1,
	bagof(Diag, Target^(
		between(NegMin, NegMax, Target),
		bagof(Cell, N^M^Col^(
			nth1(N, Matrix, Col),
			nth1(M, Col, Cell),
			Target is N - M
		), Diag)
	), NegDiags),

	append(PosDiags, NegDiags, Diags).


%! new_state(-State)
% Creates a new board state. The state is a list of 7 Columns.
% Each column has 6 rows.
new_state(State) :- new_state_(7, 6, State).

new_state_(0, _, []) :- !.
new_state_(Cols, Rows, [C|State]) :-
	length(C, Rows),
	C ins 0..1,
	Cols0 is Cols - 1,
	new_state_(Cols0, Rows, State).


%! write_state(@State)
%
write_state(State) :-
	format("  a b c d e f g\n"),
	write_state_(6, State).

write_state_(0, _) :- !.
write_state_(RowNum, State) :-
	format("~w ", [RowNum]),
	forall(member(Col, State), (
		nth1(RowNum, Col, Cell),
		(var(Cell) -> Display = "."
		;Cell == 0 -> Display = "x"
		;Cell == 1 -> Display = "o"),
		format("~w ", [Display])
	)),
	format("\n"),
	Next is RowNum - 1,
	write_state_(Next, State).
