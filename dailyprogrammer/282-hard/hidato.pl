:- use_module(library(clpfd)).

%% hidato(+Board)
% A hidato is a matrix such that all values are either the atom 'x' or a number.
% Each number must be consecutive with one of its horizontal, vertical, or
% diagonal neighbors, all numbers must be distinct, and the path of consecutive
% values must include all numbers on the board. The number 1 must be the
% smallest number on the board.
hidato(Board) :-
    Board = [FirstRow|_],
    length(FirstRow, Width),
    length(Board, Height),
    board_size(Board, Max),
    board_vals(Board, Vals),
    all_distinct(Vals),
    Vals ins 1..Max,
    between(1, Width, X),
    between(1, Height, Y),
    matrix_at(Board, X, Y, 1),
    hidato_(Board, X, Y, 1, Max),
    !.

hidato_(_, _, _, Max, Max) :- !.
hidato_(Board, X, Y, Cur, Max) :-
    X0 #= X-1, X1 #= X+1,
    Y0 #= Y-1, Y1 #= Y+1,
    C1 #= Cur+1,
    (matrix_at(Board, X,  Y0, C1), hidato_(Board, X,  Y0, C1, Max)
    ;matrix_at(Board, X,  Y1, C1), hidato_(Board, X,  Y1, C1, Max)
    ;matrix_at(Board, X0, Y,  C1), hidato_(Board, X0, Y,  C1, Max)
    ;matrix_at(Board, X0, Y0, C1), hidato_(Board, X0, Y0, C1, Max)
    ;matrix_at(Board, X0, Y1, C1), hidato_(Board, X0, Y1, C1, Max)
    ;matrix_at(Board, X1, Y,  C1), hidato_(Board, X1, Y,  C1, Max)
    ;matrix_at(Board, X1, Y0, C1), hidato_(Board, X1, Y0, C1, Max)
    ;matrix_at(Board, X1, Y1, C1), hidato_(Board, X1, Y1, C1, Max)).

%% board_size(+Board, -Max)
% Max is the maximum number for the board.
board_size(Board, Max) :- board_size(Board, Max, 0).
board_size([[]], Max, Max) :- !.
board_size([[]|T], Max, C) :-
    !,
    board_size(T, Max, C).
board_size([[H|T]|T2], Max, C) :-
    H \== x, !,
    C1 #= C+1,
    board_size([T|T2], Max, C1).
board_size([[x|T]|T2], Max, C) :-
    board_size([T|T2], Max, C).

%% board_vals(+Board, -Vals)
% Vals is the list of variables in Board sorted by the manhattan distance from
% the smallest known element. This is our ordering heuristic when labeling the
% variables.
board_vals(Board, Vals) :-
    findall(X, (
        member(Row, Board),
        member(X, Row),
        X \== x
    ), Vals).

%% matrix_at(+Board, +X, +Y, -C)
% C is the element of Board at (X,Y).
matrix_at(Board, X, Y, C) :-
    nth1(Y, Board, Row),
    nth1(X, Row, C).

%% write_board(+Board)
% Prints the board to console.
% Assumes all numbers are less than 100 for proper alignment.
write_board([[]]) :- !, write("\n").
write_board([[]|T]) :- !, write("\n"), write_board(T).
write_board([[x|T1]|T2]) :- !, write("xx "), write_board([T1|T2]).
write_board([[X|T1]|T2]) :- X < 10, !, format("0~d ", [X]), write_board([T1|T2]).
write_board([[X|T1]|T2]) :- format("~d ", [X]), write_board([T1|T2]).

%% input(+N, -Board)
% Board is the Nth challange input.
input(1, [[_ , 33, 35, _ , _ , x , x , x ],
          [_ , _ , 24, 22, _ , x , x , x ],
          [_ , _ , _ , 21, _ , _ , x , x ],
          [_ , 26, _ , 13, 40, 11, x , x ],
          [27, _ , _ , _ , 09, _ , 01, x ],
          [x , x , _ , _ , 18, _ , _ , x ],
          [x , x , x , x , _ , 07, _ , _ ],
          [x , x , x , x , x , x , 05, _ ]]).

input(2, [[_, _, 3, _, _, _, _, _],
          [x, x, x, x, x, x, x, _],
          [_, _, _, _, _, _, _, _],
          [_, x, x, x, x, x, x, x],
          [_, _, _, _, _, _, _, _],
          [x, x, x, x, x, x, x, _],
          [_, _, _, _, _, _, _, _],
          [_, x, x, x, x, x, x, x],
          [_, _, _, _, _, _, _, _]]).

input(6, [[01, _ , _ , 23, _ , _ ],
          [11, _ , 03, _ , _ , 18],
          [_ , 13, _ , _ , _ , _ ],
          [_ , _ , _ , _ , 26, _ ],
          [08, _ , _ , 15, _ , 30],
          [_ , _ , 36, _ , _ , 31]]).
