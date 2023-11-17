---
title: Solving Puzzles with Prolog
---

# Defining the Game

## The Puzzle Spec

A puzzle is naturally represented as a list of bottles, where each bottle is a
list of balls and each ball is an atom giving the color. We'll order the balls
so that the first ball in the list gives the top of the bottle.

Let's define a predicate `puzzle/2` to serve as the puzzle database.

```prolog
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
```

## The Game State

The list-of-lists representation of the puzzle is natural and easy to work
with, but it is not the only thing we need to track in our solver. Consider the
following:

- **Observation 1**: At the end of our search for a solution, what we actually
  care about is how we arrived at the solution. The easiest way to present this
  information is to record every state in the history from the start state to
  the solution.

- **Observation 2**: During the search for the solution, we need to perform
  loop checking to ensure we do not re-enter a state we have already visited.
  Imagine bouncing a ball back and forth between two bottles. Again, this
  means that we need to record all of the states we visit.

- **Observation 3**: Many states are effectively the same state. Imagine if we
  transpose any two bottles; that would essentially be the same state. To trim
  the search space, we want to *canonicalize* states so that two states which
  are effectively the same have the same canonical representation. We can
  canonicalize states simply by sorting the outer list.

Using these observations, we can define a "game state" to track all of this
information during our search. Let's represent this game state as a term,
`game(History, Canonicals)`, where `History` is the list of states visited
(most recent first), and `Canonicals` is the same list except each state is in
its canonical form.

From here, we can define a helper predicate `start_game/2` which wraps a puzzle
spec into an initial game state.

```prolog
%% start_game(?Id, ?Game).
% Get an initial game state for a puzzle.
%
% Parameters:
%   Id: The puzzle identifier.
%   Game: The initial game state.
start_game(Id, Game) :-
    Game = game([Puzzle], [Canonical]),
    puzzle(Id, Puzzle),
    msort(Puzzle, Canonical).
```

## Advancing the State

Now that we've defined a game state representation, the next step is to define
how the game is played. We'll do this by writing a predicate `advance/2` to act
as a successor function. It will have two arguments: the first being the
previous state and the second being the successor state. The predicate should
be non-deterministic so that we can generate all possible successor state.

The algorithm for our successor function is simple. We select two bottles from
the previous state, called the "take-bottle" and the "put-bottle". All bottles
of the successor state are exactly the same as the previous state except that
the top ball of the take-bottle is moved to the top of the put-bottle. Once we
create the successor state, we'll do a loop-check to ensure that we are not
returning to a previously visited state.

There are a few invariants that we need to enforce in this process. (1) The
top ball of the put-bottle must be the same color as the top ball of the
take-bottle, or the put-bottle must be empty. (2) The put-bottle must have an
open space, i.e. three balls or less. And (3) we shouldn't be taking from a
bottle that is already "solved".

```prolog
%% advance(+Prev, -Next).
% Advance a game by one step.
%
% Parameters:
%   Prev: The previous game state.
%   Next: The subsequent game state.
advance(Prev, Next) :-
    % Next is like Prev with the new puzzle state prepended to the lists.
    Prev = game([PrevPuzzle|History], Canonicals),
    Next = game([NextPuzzle,PrevPuzzle|History], [NextCanonical|Canonicals]),

    % Select a "take-bottle" and a "put-bottle".
    PrevTakeBottle = [X|NextTakeBottle],
    NextPutBottle = [X|PrevPutBottle],
    (PrevPutBottle = [X|_] ; PrevPutBottle = []),
    select_two(
        PrevTakeBottle, PrevPutBottle, PrevPuzzle,
        NextTakeBottle, NextPutBottle, NextPuzzle
    ),

    % The take-bottle must not be a complete bottle.
    % The put-bottle must not already be full.
    \+ PrevTakeBottle = [X,X,X,X],
    \+ PrevPutBottle = [_,_,_,_],

    % Define the canonical state.
    msort(NextPuzzle, NextCanonical),

    % Do not loop back to a previous state.
    \+ member(NextCanonical, Canonicals).


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
```

And finally, we'll define `done/1` to check when a game is solved, i.e. all
bottles contain exactly four balls of the same color or are totally empty.

```prolog
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
```
