:- use_module(library(clpfd)).

%% matrix(?M, ?N, ?Matrix)
% Matrix is an MxN matrix.
matrix(M, N, Matrix) :-
    length(Matrix, M),
    maplist({N}/[Row] >> length(Row, N), Matrix).


%% partition_ints(+N, +M, ?Parts)
% Partition the integers from 1 through N*M into N subsets of size M.
partition_ints(N, M, Parts) :-
    % Ensure Parts is a 2D list.
    matrix(N, M, Parts),

    % Ensure all the elements are distinct.
    append(Parts, Vals),
    Count is N * M,
    Vals ins 1..Count,
    all_distinct(Vals),

    % Sets in Prolog are just sorted lists.
    lex_chain(Parts),  % Ensure Parts is sorted.
    maplist([Row] >> chain(Row, #<), Parts),  % Ensure each subset is sorted.

    % Label to get concrete values.
    label(Vals).
