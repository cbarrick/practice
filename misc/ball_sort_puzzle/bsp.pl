use_module(library(ordsets)).


% Puzzles
% ---------------------------------------------------------------------------

%% puzzle(?Id, ?PuzzleSpec).
% The database of puzzles.
%
% Parameters:
%   Id: The puzzle identifier. Typically the puzzle number.
%   PuzzleSpec: A specification of the initial state of the puzzle.
puzzle(106, [
    [bl,hu,lb,lb],  % Colors
    [pu,pi,ol,si],  % ----------
    [or,pu,re,br],  % bl = Blue
    [or,pi,re,or],  % hu = Hunter green
    [hu,re,ye,bl],  % lb = Light blue
    [ye,hu,br,hu],  % pu = Purple
    [br,pu,re,li],  % pi = Pink
    [li,pu,pi,li],  % ol = Olive
    [ol,si,lb,bl],  % si = Silver
    [br,ye,si,ol],  % or = Orange
    [si,ye,li,bl],  % re = Red
    [ol,lb,pi,or],  % br = Brown
    [],             % ye = Yellow
    []              % li = Lime
]).


% Helpers
% ---------------------------------------------------------------------------

%% select_two(?X1, ?X2, ?XList, ?Y1, ?Y2, ?YList).
% XList is like YList except element X1 is replaced by Y1 and X2 is replaced
% by Y2. This is like the builtin select/4 except two elements are selected.
%
% Parameters:
%   X1: Any element of XList, at the same position as Y1.
%   X2: Any element of XList, at the same position as Y2.
%   XList: Like YList, except Y1 and Y2 are replaced by X1 and X2.
%   Y1: Any element of YList, at the same position as X1.
%   Y2: Any element of YList, at the same position as X2.
%   YList: Like XList, except X1 and X2 are replaced by Y1 and Y2.
select_two(X1, X2, XList, Y1, Y2, YList) :-
    nth1(N1, XList, X1, XTemp),
    nth1(N1, YList, Y1, YTemp),
    nth1(N2, XTemp, X2, Rest),
    nth1(N2, YTemp, Y2, Rest).


%% parallelism(?N).
% The amount of parallelism to use in the parallel solver.
%
% Parameters:
%   N: A positive integer number of parallelism.
parallelism(10).


% Game Definition
% ---------------------------------------------------------------------------
% We take about the puzzle in terms of "game states" and "board states". The
% board state is the state of the puzzle board at any point in time. The game
% state consists of a history of all board states visited during the course of
% play, along with additional data used by the solver.
%
% A board state is a list of lists. Each inner list represents a tube, and each
% element of the tube list is a term representing a ball color. The first
% element of the tube list is the top of the tube. Each board state has a
% "canonical" form, which is just the sorting of the outer list. This allows
% multiple board states to have the same canonical form and helps cut the
% search space.
%
% A game state is defined by the term `game(History, Seen)`, where `History` is
% the list of board states for the game (most recent first), and `Seen` is an
% ordset containing the canonical form of all elements of the `History`.

%% start_game(?Id, ?Game).
% Get an initial game state for a puzzle.
%
% Parameters:
%   Id: The puzzle identifier.
%   Game: The initial game state.
start_game(Id, game([Puzzle], [Canonical])) :-
    puzzle(Id, Puzzle),
    msort(Puzzle, Canonical).


%% done(+Game).
% True when Game represents a completed game.
%
% Parameters:
%   Game: A game state.
done(Game) :-
    Game = game([State|_], _),
    once(done_(State)).

done_([[X,X,X,X]|T]) :- !, done_(T).
done_([[]|T]) :- !, done_(T).
done_([]).


%% advance(+Prev, -Next).
% Advance a game by one step.
%
% Parameters:
%   Prev: The previous game state.
%   Next: The subsequent game state.
advance(Prev, Next) :-
    % Next is like Prev with the new puzzle state prepended to the lists.
    Prev = game([PrevPuzzle|History], Canonicals),
    Next = game([NextPuzzle,PrevPuzzle|History], [Canonical|Canonicals]),

    % Select a "take bottle" and a "put bottle".
    PrevTakeBottle = [X|NextTakeBottle],
    NextPutBottle = [X|PrevPutBottle],
    (PrevPutBottle = [X|_] ; PrevPutBottle = []),
    select_two(
        PrevTakeBottle, PrevPutBottle, PrevPuzzle,
        NextTakeBottle, NextPutBottle, NextPuzzle
    ),

    % The take bottle must not be a complete bottle.
    % The put bottle must not already be full.
    \+ PrevTakeBottle = [X,X,X,X],
    \+ PrevPutBottle = [_,_,_,_],

    % Define the canonical state.
    msort(NextPuzzle, Canonical),

    % Do not loop back to a previous state.
    \+ member(Canonical, Canonicals).



% Solver - Depth First Search
% ---------------------------------------------------------------------------

%% solve_dfs(+StartState, -Solution).
% A depth-first solver.
solve_dfs(StartState, Solution) :-
    start_game(StartState, StartGame),
    once(solve_dfs_(StartGame, Solution)).

solve_dfs_(EndGame, EndGame) :- done(EndGame).
solve_dfs_(StartGame, EndGame) :-
    advance(StartGame, NextGame),
    solve_dfs_(NextGame, EndGame).


% Solver - Parallel Breadth First Search
% ---------------------------------------------------------------------------

:- dynamic solve_bfs__seen_/1.
:- dynamic solve_bfs__soln_/1.
:- dynamic solve_bfs__queue_/1.

%% solve_bfs(+StartState, -Solution).
% A parallel breadth-first solver.
solve_bfs(StartState, Solution) :-
    % Clear the queues.
    retractall(solve_bfs__seen_(_)),
    retractall(solve_bfs__soln_(_)),
    retractall(solve_bfs__queue_(_)),

    % Assert a new game state into the queue.
    start_game(StartState, StartGame),
    assertz(solve_bfs__queue_(StartGame)),

    % Create-and-join the treads.
    parallelism(N),
    length(Threads, N),
    maplist(thread_create(solve_bfs__worker_), Threads),
    maplist(thread_join, Threads),

    % Read the solution.
    once(solve_bfs__soln_(Solution)).


%% solve_bfs__worker_.
% The main breadth-first search worker loop.
solve_bfs__worker_ :-
    solve_bfs__soln_(_),
    !.

solve_bfs__worker_ :-
    (
        % Pop a game from the queue.
        retract(solve_bfs__queue_(Game)),
        Game = game(_, [Canonical|_]),
        \+ solve_bfs__seen_(Canonical),
        assertz(solve_bfs__seen_(Canonical))
    ->
        % Expand future states into the queue.
        solve_bfs__expand_(Game)
    ).

solve_bfs__worker_ :- solve_bfs__worker_.


%% solve_bfs__expand_(+Game).
% Do one branch of breadth-first search.
% Succeeds if the game is a solution, and asserts the solution.
% Otherwise asserts successor games into the queue and fails.
solve_bfs__expand_(Game) :-
    done(Game),
    !,
    assertz(solve_bfs__soln_(Game)).
solve_bfs__expand_(Game) :-
    forall(advance(Game, NextGame), assertz(solve_bfs__queue_(NextGame))),
    !,
    fail.


% I/O
% ---------------------------------------------------------------------------

%% print_game(+Game).
% Prints a game to the current output.
print_game(Game) :-
    Game = game(History, _),
    print_history_(History).

print_history_([]) :- write("---"), nl.
print_history_([State|Rest]) :-
    write("---"), nl,
    print_state_(State),
    print_history_(Rest).

print_state_([]).
print_state_([H|T]) :-
    reverse(H, Bottle),
    write(Bottle), nl,
    print_state_(T).


%% solve_and_print(+InitState).
% Solve a game given an initial bord state, then print the solution.
solve_and_print(InitState) :-
    solve_bfs(InitState, Solution),
    print_game(Solution).


% Main
% ---------------------------------------------------------------------------

main :-
    puzzle(106, StartState),
    solve_and_print(StartState),
    !.
