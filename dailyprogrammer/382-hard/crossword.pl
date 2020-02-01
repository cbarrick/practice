:- use_module(library(clpfd)).


%% input(?Name, ?Height, ?Width, ?HClueSet, ?VClueSet)
% The challenge inputs.
%
% Arguments:
%     Name (atom): The name of the challenge.
%     Height (int): The number of rows in the crossword.
%     Width (int): The number of columns in the crossword.
%     HClueSet (ordset of int): The set of clues with a horizontal component.
%     VClueSet (ordset of int): The set of clues with a vertical component.
input(
    simple,
    9,
    9,
    [1, 4, 7, 9, 10, 12, 13, 14, 15, 17, 20, 22, 23, 24, 25],
    [1, 2, 3, 4, 5, 6, 7, 8, 11, 14, 15, 16, 17, 18, 19, 20, 21]
).

input(
    example,
    15,
    15,
    [1, 4, 7, 10, 13, 14, 16, 17, 18, 20, 21, 23, 24, 26, 28, 29, 33, 35, 36,
        38, 39, 42, 44, 45, 47, 49, 50, 52, 55, 56, 58, 59, 61, 63, 67, 69, 70,
         71, 72, 73, 74, 75, 76],
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 15, 19, 22, 25, 27, 29, 30, 31, 32, 34,
        37, 40, 41, 43, 46, 48, 51, 53, 54, 57, 60, 62, 64, 65, 66, 68]
).

input(
    ch1,
    15,
    15,
    [1, 6, 10, 12, 13, 14, 16, 17, 19, 20, 21, 23, 25, 27, 29, 30, 31, 33, 34,
        35, 37, 38, 40, 41, 42, 44, 45, 46, 49, 50],
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 17, 18, 20, 21, 22, 24, 26, 28,
        32, 33, 36, 39, 41, 43, 45, 47, 48]
).

input(
    ch2,
    21,
    21,
    [1, 4, 7, 10, 12, 14, 16, 17, 18, 19, 21, 24, 25, 26, 27, 29, 30, 32, 34,
        35, 36, 38, 39, 40, 42, 45, 46, 48, 49, 51, 52, 54, 55, 56, 57, 58, 59,
        61, 63, 64, 66, 67, 69, 70, 73, 74, 75, 76, 77, 79, 81, 82, 84, 85, 87,
        89, 90, 92, 94, 96, 97, 99, 100, 101, 102, 103, 104, 105],
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 19, 20, 22, 23, 24, 28,
        31, 33, 37, 38, 41, 42, 43, 44, 45, 47, 50, 52, 53, 60, 62, 63, 64, 65,
        67, 68, 71, 72, 74, 78, 80, 81, 83, 84, 86, 88, 89, 91, 93, 95, 98]
).

input(
    ch3,
    27,
    27,
    [1, 5, 10, 15, 18, 22, 25, 27, 29, 32, 33, 34, 35, 36, 37, 38, 39, 40, 42,
        44, 46, 47, 48, 49, 51, 52, 54, 55, 56, 57, 59, 61, 65, 67, 69, 70, 71,
        73, 74, 77, 80, 82, 84, 86, 87, 88, 89, 91, 93, 94, 96, 99, 101, 102,
        103, 104, 106, 108, 110, 112, 114, 115, 116, 119, 121, 123, 125, 126,
        128, 129, 132, 133, 135, 136, 138, 139, 140, 142, 144, 147, 148, 149,
        151, 153, 154, 156, 158, 162, 166, 167, 169, 170, 171, 173, 174, 176,
        177, 178, 179, 181, 182, 185, 186, 187, 188, 189, 191, 192, 193, 195,
        201, 202, 203, 204, 205, 206, 207, 208],
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
        23, 24, 26, 28, 30, 31, 37, 39, 40, 41, 43, 45, 46, 47, 50, 53, 54, 55,
        56, 58, 60, 62, 63, 64, 66, 68, 72, 74, 75, 76, 78, 79, 81, 83, 85, 88,
        90, 92, 94, 95, 97, 98, 100, 103, 105, 106, 107, 109, 111, 113, 117,
        118, 120, 122, 124, 127, 128, 130, 131, 134, 137, 139, 141, 143, 144,
        145, 146, 148, 150, 152, 155, 157, 159, 160, 161, 163, 164, 165, 168,
        172, 175, 176, 177, 178, 180, 181, 183, 184, 185, 186, 190, 192, 193,
        194, 196, 197, 198, 199, 200]
).

input(
    ch4,
    33,
    33,
    [1, 5, 13, 19, 23, 25, 28, 29, 30, 33, 34, 35, 36, 37, 38, 42, 43, 48, 49,
        50, 51, 56, 58, 60, 61, 62, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 76,
        78, 79, 80, 81, 83, 86, 88, 89, 90, 93, 94, 96, 100, 101, 102, 105, 107,
        108, 109, 110, 111, 112, 114, 116, 117, 118, 120, 121, 122, 126, 127,
        128, 130, 131, 133, 134, 135, 138, 139, 141, 142, 143, 145, 146, 149,
        151, 152, 154, 155, 159, 160, 163, 165, 166, 167, 168, 172, 173, 174,
        175, 176, 178, 180, 181, 183, 185, 187, 188, 189, 190, 191, 192, 193,
        194, 197, 199, 200, 201, 202, 203, 205, 207, 208, 209, 210, 212, 213,
        215, 216, 217, 220, 222, 223, 224, 226, 227, 228, 230, 233, 234, 235,
        236, 238, 242, 244, 246, 247, 248, 249, 250, 252, 253, 254, 255, 256,
        259, 260, 263, 265, 271, 276, 277, 278, 279, 280, 281, 282],
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
        22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 38, 39, 40, 41, 42, 43,
        44, 45, 46, 47, 51, 52, 53, 54, 55, 56, 57, 59, 61, 63, 73, 74, 75, 77,
        82, 83, 84, 85, 87, 88, 89, 90, 91, 92, 95, 97, 98, 99, 100, 101, 102,
        103, 104, 106, 110, 113, 115, 116, 118, 119, 123, 124, 125, 126, 127,
        129, 132, 134, 135, 136, 137, 140, 142, 143, 144, 147, 148, 149, 150,
        151, 153, 154, 156, 157, 158, 159, 160, 161, 162, 164, 167, 168, 169,
        170, 171, 176, 177, 178, 179, 182, 184, 185, 186, 187, 192, 194, 195,
        196, 198, 201, 202, 204, 206, 208, 209, 211, 213, 214, 215, 216, 217,
        218, 219, 221, 223, 225, 226, 229, 231, 232, 233, 237, 239, 240, 241,
        243, 244, 245, 248, 251, 253, 255, 257, 258, 259, 260, 261, 262, 263,
        264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275]
).


%% print_crossword(+Xword)
% Print a (partial) crossword grid.
%
% Arguments:
%     Xword (binary matrix): The crossword grid.
print_crossword([]) :- !, write("\n").
print_crossword([[]|Xword]) :- !, write("\n"), print_crossword(Xword).
print_crossword([[V|Row]|Xword]) :- var(V), !, write("_ "), print_crossword([Row|Xword]).
print_crossword([[0|Row]|Xword]) :- !, write("# "), print_crossword([Row|Xword]).
print_crossword([[1|Row]|Xword]) :- !, write(". "), print_crossword([Row|Xword]).


%% matrix(?Height, ?Width, ?Grid)
% `Grid` is a matrix with `Height` rows and `Width` columns.
%
% Arguments:
%     Height (int): The number of rows in the matrix.
%     Width (int): The number of columns in the matrix.
%     Grid (list of list): The matrix.
matrix(H, W, Grid) :-
    length(Grid, H),
    maplist({W}/[Row] >> length(Row, W), Grid).


%% index_grid(?Pos, ?Grid, ?Val)
% Pos is a `[Y, X]` position in `Grid` and `Val` is the value at that position.
%
% Arguments:
%     Pos (list of int): A `[Y, X]` pair indexing into the grid.
%     Grid (matrix): The grid being indexed.
%     Val (any): The value in the grid at the position.
index_grid([Y, X], Grid, Val) :-
    matrix(_, _, Grid),
    nth1(Y, Grid, Row),
    nth1(X, Row, Val).


%% index_grid_up(?Pos, ?Grid, ?Val)
% Like `index_grid/3`, but `Val` is the value above the position.
% `Val` is 0 if the position is along the top edge.
%
% Arguments:
%     Pos (list of int): A `[Y, X]` pair indexing into the grid.
%     Grid (matrix): The grid being indexed.
%     Val (any): The value in the grid above the position.
index_grid_up([1, X], Grid, 0) :-
    matrix(_, Width, Grid),
    between(1, Width, X).

index_grid_up([Y, X], Grid, Val) :-
    matrix(Height, _, Grid),
    between(2, Height, Y),
    Y0 is Y - 1,
    index_grid([Y0, X], Grid, Val).


%% index_grid_left(?Pos, ?Grid, ?Val)
% Like `index_grid/3`, but `Val` is the value to the left of the position.
% `Val` is 0 if the position is along the left edge.
%
% Arguments:
%     Pos (list of int): A `[Y, X]` pair indexing into the grid.
%     Grid (matrix): The grid being indexed.
%     Val (any): The value in the grid left of the position.
index_grid_left([Y, 1], Grid, 0) :-
    matrix(Height, _, Grid),
    between(1, Height, Y).

index_grid_left([Y, X], Grid, Val) :-
    matrix(_, Width, Grid),
    between(2, Width, X),
    X0 is X - 1,
    index_grid([Y, X0], Grid, Val).


%% min_run_3(?List).
% The 1s allways occur in groups of at least three.
%
% Arguments:
%     List (list of int): A list of 1s and 0s.
min_run_3([A, B, C | List]) :-
    min_run_3__loop([0, A, B, C | List]).

min_run_3__loop([_, _, _]).
min_run_3__loop([A, B, C, D | List]) :-
    ((A #= 0) #/\ (B #= 1)) #==> ((C #= 1) #/\ (D #= 1)),
    min_run_3__loop([B, C, D | List]).


%% mapgrid(:Goal, +Grid)
% Call `Goal([Y, X], Val)` for each `[Y, X]` position in the grid and `Value`
% at that position.
%
% Argument:
%     Goal: The goal to call at each position.
%     Grid (matrix): The grid to map over.
mapgrid(Goal, Grid) :-
    once(matrix(Height, Width, Grid)),
    append(Grid, Values),
    findall([Y, X], (
        between(1, Height, Y),
        between(1, Width, X)
    ), Positions),
    maplist(Goal, Positions, Values).


%% clue_grid(?Xword, ?ClueGrid).
% Relateas a binary grid representing a crosswoard puzzle to a quadrary grid
% indicating cells that are the start of words.
%
% Cells of the clue grid have the following values:
%
% - 0: Not the start of a word.
% - 1: The start of a horizontal word.
% - 2: The start of a vertical word.
% - 3: The start of both a horizontal and vertical word.
%
% Arguments:
%     Xword (binary matrix):
%         A crossword grid with 0 for black cells and 1 for white cells.
%     ClueGrid (quadrary matrix):
%         A grid the same size as `Xword` with values indicating the clue
%         status of cells in the crossword.
clue_grid(Xword, ClueGrid) :-
    matrix(Height, Width, Xword),

    % HClueGrid is a binary grid for horizontal clues.
    matrix(Height, Width, HClueGrid),
    mapgrid({Xword, HClueGrid} / [Pos, Color] >> (
        index_grid(Pos, HClueGrid, HClue),
        index_grid_left(Pos, Xword, LeftColor),
        Color #= 0 #==> HClue #= 0,
        LeftColor #= 1 #==> HClue #= 0,
        LeftColor #= 0 #/\ Color #= 1 #<==> HClue #= 1
    ), Xword),

    % VClueGrid is a binary grid for vertical clues.
    matrix(Height, Width, VClueGrid),
    mapgrid({Xword, VClueGrid} / [Pos, Color] >> (
        index_grid(Pos, VClueGrid, VClue),
        index_grid_up(Pos, Xword, UpColor),
        Color #= 0 #==> VClue #= 0,
        UpColor #= 1 #==> VClue #= 0,
        UpColor #= 0 #/\ Color #= 1 #<==> VClue #= 1
    ), Xword),

    % ClueGrid is computed from HClueGrid and VClueGrid.
    matrix(Height, Width, ClueGrid),
    mapgrid({HClueGrid, VClueGrid} / [Pos, Clue] >> (
        index_grid(Pos, HClueGrid, HClue),
        index_grid(Pos, VClueGrid, VClue),
        Clue #= HClue + 2*VClue
    ), ClueGrid).


%% clue_constraints(+Xword, ++HClueSet, ++VClueSet)
% Apply the numbering constraints to a crossword grid.
%
% This predicate transforms the clue constraints into a finite automaton that
% constrains the flattened clue grid.
%
% Arguments:
%     Xword (binary matrix): A crossword grid, 0 for black and 1 for white.
%     HClueSet (ordset of int): The set of clues with a horizontal component.
%     VClueSet (ordset of int): The set of clues with a vertical component.
clue_constraints(Xword, HClueSet, VClueSet) :-
    clue_grid(Xword, ClueGrid),
    append(ClueGrid, ClueFlat),
    clue_constraints__make_dfa([0|HClueSet], [0|VClueSet], Arcs, Sink),
    automaton(ClueFlat, [source([0,0]), sink(Sink)], Arcs).


clue_constraints__make_dfa([H], [V], Arcs, [H,V]) :-
    !,
    findall(arc([A,B], 0, [A,B]), (
        between(0, H, A),
        between(0, V, B)
    ), Arcs).

clue_constraints__make_dfa([HA,HB|HCS], [VA,VB|VCS], [Arc|Arcs], Sink) :-
    HB = VB,
    !,
    Arc = arc([HA,VA], 3, [HB,VB]),
    clue_constraints__make_dfa([HB|HCS], [VB|VCS], Arcs, Sink).

clue_constraints__make_dfa([HA,HB|HCS], [VA,VB|VCS], [Arc|Arcs], Sink) :-
    HB < VB,
    !,
    Arc = arc([HA,VA], 1, [HB,VA]),
    clue_constraints__make_dfa([HB|HCS], [VA,VB|VCS], Arcs, Sink).

clue_constraints__make_dfa([HA,HB|HCS], [VA,VB|VCS], [Arc|Arcs], Sink) :-
    HB > VB,
    !,
    Arc = arc([HA,VA], 2, [HA,VB]),
    clue_constraints__make_dfa([HA,HB|HCS], [VB|VCS], Arcs, Sink).

clue_constraints__make_dfa([HA,HB|HCS], [VA], [Arc|Arcs], Sink) :-
    !,
    Arc = arc([HA,VA], 1, [HB,VA]),
    clue_constraints__make_dfa([HB|HCS], [VA], Arcs, Sink).

clue_constraints__make_dfa([HA], [VA,VB|VCS], [Arc|Arcs], Sink) :-
    !,
    Arc = arc([HA,VA], 2, [HA,VB]),
    clue_constraints__make_dfa([HA], [VB|VCS], Arcs, Sink).


%% parallelism(-N)
% The number of threads to use in the search.
%
% Arguments:
%     N (int): Either the value of the THREADS environment variable or 4.
parallelism(N) :-
    getenv('THREADS', NAtom),
    atom_string(NAtom, NStr),
    number_string(N, NStr),
    !.
parallelism(4).


%% thread_pool_join(+Pool, -Status)
% Join all threads in a pool.
%
% Arguments:
%     Pool (atom): The identifier of the thread pool.
%     Status (any): The expected status of the threads, see `thread_join/2`.
thread_pool_join(Pool, Status) :-
    repeat,
    thread_pool_property(Pool, members(Threads)),
    maplist({Status} / [T] >> thread_join(T, Status), Threads),
    thread_pool_property(Pool, running(0)),
    !.


%% label_crossword(+Xword)
% Label the crossword in parallel.
%
% Arguments:
%     Xword (binary matrix): The crossword grid to label.
label_crossword(Xword) :-
    % The call is may return multiple solutions upon backtracking. Worker
    % threads will continue to search in the background, even after an initial
    % solution is found, until choice is committed (see `setup_call_cleanup/3`).
    setup_call_cleanup(
        label_crossword__setup(Pool, Ret, Die),
        label_crossword__call(Xword, Pool, Ret, Die),
        label_crossword__cleanup(Pool, Ret, Die)
    ).


%% label_crossword__setup(-Pool, -Ret, -Die)
% Setup the pieces needed for the parallel search.
%
% Arguments:
%     Pool (atom): An identifier for a newly created thread pool.
%     Ret (channel): A channel for the workers to return solutions.
%     Die (channel): A channel to signal that the workers should die.
label_crossword__setup(Pool, Ret, Die) :-
    % The backlog must be at least N - 2 to avoid deadlock:
    % - The degree of parallelism is N.
    % - For a thread to fork, it must detect that the pool has a free slot.
    % - The worst case is that there is 1 free slot and N-1 running threads
    %   that all decide to fork at the same time.
    % - This leads to 2(N-1) threads being scheduled on the pool.
    % - The backlog is thus 2(N-1)-N, or N - 2.
    gensym(worker_pool_, Pool),
    parallelism(N),
    message_queue_create(Ret, [max_size(1)]),
    message_queue_create(Die, [max_size(1)]),
    thread_pool_create(Pool, N, [backlog(N)]).


%% label_crossword__cleanup(+Pool, +Ret, +Die)
% Cleanup the thread pool.
%
% Arguments:
%     Pool (atom): The identifier of the thread pool.
%     Ret (channel): The channel for the workers to return solutions.
%     Die (channel): The channel to signal that the workers should die.
label_crossword__cleanup(Pool, Ret, Die) :-
    thread_send_message(Die, die),
    thread_pool_join(Pool, exited(killed)),
    thread_pool_destroy(Pool),
    message_queue_destroy(Die),
    message_queue_destroy(Ret).


%% label_crossword__call(+Xword, +Pool, +Ret, +Die)
% Create an initial worker, then listen for solutions over the return channel.
%
% Arguments:
%     Xword (binary matrix): The crossword grid to label.
%     Pool (atom): The identifier of the thread pool.
%     Ret (channel): The channel for the workers to return solutions.
%     Die (channel): The channel to signal that the workers should die.
label_crossword__call(Xword, Pool, Ret, Die) :-
    % Spawn an initial worker thread in the pool. The worker will label `Xword`
    % and spawn additional workers to saturate the pool.
    append(Xword, Vars),
    Goal = label_crossword__worker(Vars, Pool, Ret, Die, Xword),
    thread_create_in_pool(Pool, Goal, _, []),

    % Repeatedly pull solutions from the return channel. Once per second, check
    % that worker threads are still running. If not, then no solution exists.
    repeat,
    (thread_pool_property(Pool, running(0)) ->
        !, fail
    ;
        thread_get_message(Ret, Xword, [timeout(1)])
    ).


%% label_crossword__worker(+Vars, +Pool, +Ret, +Die, ?Xord)
% The predicate run by the worker threads.
%
% Arguments:
%     Vars (list of int): The variables remaining to be labeled.
%     Ret (channel): The channel for the workers to return solutions.
%     Die (channel): The channel to signal that the workers should die.
%     Xword (binary matrix): The crossword grid to label.
label_crossword__worker(_, _, _, Die, _) :-
    % Check if the thread can die. Otherwise backtrack.
    thread_peek_message(Die, _),
    thread_exit(killed).

label_crossword__worker([], _, Ret, Die, XW) :-
    % We successfully labeled all of the cells. Pass the solution on the return
    % channel, then backtrack for more solutions. Sending will block if Ret is
    % full. To avoid deadlock, we periodically check the Die channel.
    repeat,
    (thread_peek_message(Die, _) -> thread_exit(killed) ; true),
    thread_send_message(Ret, XW, [timeout(1)]),
    !,
    fail.

label_crossword__worker([X|Vars], Pool, Ret, Die, XW) :-
    % Determine if we want to fork a new thread.
    var(X),
    \+ thread_pool_property(Pool, free(0)),
    !,

    % Fork the case where X=0 into a different thread.
    thread_create_in_pool(Pool, (
        X = 0,
        label_crossword__worker(Vars, Pool, Ret, Die, XW)
    ), _, []),

    % Consider the case where X=1 in this thread.
    X = 1,
    label_crossword__worker(Vars, Pool, Ret, Die, XW).

label_crossword__worker([X|Vars], Pool, Ret, Die, XW) :-
    % Consider both cases X=0 and X=1 in this thread.
    % Randomizing the search order leads to noticable gains.
    (maybe(0.5) -> SearchOrder = [1,0] ; SearchOrder = [0,1]),
    (var(X) -> member(X, SearchOrder) ; integer(X)),
    label_crossword__worker(Vars, Pool, Ret, Die, XW).


%% connected_chk(?Xword)
% Verify that the white cells of a crossword form a single connected component.
% The input must contain at least one white cell.
%
% There is little value in defining a connectedness constraint that is applied
% before the search. It would introduce a ton of constraints, tanking the
% performance of the constraint propogation. Additionally, in the presence of
% the other constraints, very few solutions violate the connectedness rule
% anyway. Therefore, this is implemented as a simple check to be applied after
% the search; the puzzle must already be ground.
%
% This check works in a loop. It starts by replacing the first 1 with a 2.
% Then some 1 adjacent to a 2 is replaced by a 2. This repeats until no more
% replacements can be made. If no more 1s remain, the original input must be
% connected.
%
% Arguments:
%     Xword (binary matrix): A crossword grid, 0 for black and 1 for white.
connected_chk(Xword) :-
    matrix(Height, Width, Xword),
    matrix(Height, Width, Next),
    append(Xword, XwordFlat),
    append(Next, NextFlat),
    once(nth1(N, XwordFlat, 1, Rest)),
    once(nth1(N, NextFlat, 2, Rest)),
    once(connected_chk_(Next)).

connected_chk_(Xword) :-
    \+ index_grid(_, Xword, 1).

connected_chk_(Xword) :-
    matrix(Height, Width, Xword),
    matrix(Height, Width, Next),

    index_grid(Pos, Xword, 2),
    connected_chk__adjacent(Pos, Adj),
    index_grid(Adj, Xword, 1),

    connected_chk__flat_pos(Height, Adj, AdjFlat),
    append(Xword, XwordFlat),
    append(Next, NextFlat),
    nth1(AdjFlat, XwordFlat, 1, Rest),
    nth1(AdjFlat, NextFlat, 2, Rest),
    connected_chk_(Next).

connected_chk__adjacent([Y, X1], [Y, X2]) :- X2 #= X1 + 1.
connected_chk__adjacent([Y, X1], [Y, X2]) :- X2 #= X1 - 1.
connected_chk__adjacent([Y1, X], [Y2, X]) :- Y2 #= Y1 + 1.
connected_chk__adjacent([Y1, X], [Y2, X]) :- Y2 #= Y1 - 1.

connected_chk__flat_pos(Height, [Y, X], PosFlat) :-
    PosFlat #= (Height * (Y - 1)) + (X - 1) + 1.


%% crossword(?Height, ?Width, +HClueSet, +VClueSet, ?Xword)
% Generate a crossword grid.
%
% A crossword is a binary matrix where 0 represents black cells and 1
% represents white cells. A crossword has 180 degree rotational symmetry, and
% the white cells for a single connected component. Each word is at least
% three letters long.
%
% A crossword is associated with a set of clues. A clue is assigned to each
% cell which is the first letter of either a horizontal word, a vertical word,
% or both. The clues are numbered left-to-right, top-to-bottom, starting at 1.
% All cells which are the start of words are aisgned clues and no two clues are
% assigned to the same cell.
%
% Arguments:
%     Height (int): The number of rows in the crossword.
%     Width (int): The number of columns in the crossword.
%     HClueSet (ordset of int): The set of clues with a horizontal component.
%     VClueSet (ordset of int): The set of clues with a vertical component.
%     Xword (binary matrix): A crossword grid, 0 for black and 1 for white.
crossword(Height, Width, HClueSet, VClueSet, Xword) :-
    % Xword is a binary matrix; 0 for black cells, 1 for white cells.
    matrix(Height, Width, Xword),
    append(Xword, Vars),
    Vars ins 0..1,

    % The grid has 180 degree rotational symmetry.
    reverse(Vars, Vars),

    % Every word is at least 3 letters long.
    transpose(Xword, XwordT),
    once(maplist(min_run_3, Xword)),
    once(maplist(min_run_3, XwordT)),

    % Constrain the crossword according to the clue sets.
    clue_constraints(Xword, HClueSet, VClueSet),

    % Do a parallel depth first search.
    label_crossword(Xword),

    % Verify that the white cells form a single connected component.
    connected_chk(Xword).


%% run(+Name)
% Run the solver on the challenge of the given name then print the solution
% along with runtime statistics.
%
% Note that the CPU utilization (and LIPS?) printed only count the main thread,
% which spends most of its time waiting on the worker pool. Rest assured that
% your CPU is maxed out up to the number of threads specified in the `THREADS`
% environment variable.
%
% Arguments:
%     Name (atom): The name of the challenge input to use.
run(Name) :-
    input(Name, Height, Width, HClueSet, VClueSet),
    time(crossword(Height, Width, HClueSet, VClueSet, Xword)),
    print_crossword(Xword),
    !.
